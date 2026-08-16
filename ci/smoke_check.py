#!/usr/bin/env python3
"""Spawn a built sextant binary and verify it speaks LSP over stdio."""
import json
import subprocess
import sys
import threading
import time

READ_TIMEOUT = 10.0
SHUTDOWN_GRACE = 3.0


class Framer:
    def __init__(self, proc):
        self.proc = proc
        self.buf = bytearray()
        self.cond = threading.Condition()
        self.eof = False
        self.thread = threading.Thread(target=self._pump, daemon=True)
        self.thread.start()

    def _pump(self):
        while True:
            chunk = self.proc.stdout.read1(4096)
            with self.cond:
                if not chunk:
                    self.eof = True
                    self.cond.notify_all()
                    return
                self.buf.extend(chunk)
                self.cond.notify_all()

    def _try_extract(self):
        sep = self.buf.find(b"\r\n\r\n")
        if sep == -1:
            return None
        header = self.buf[:sep].decode("ascii", errors="replace")
        length = None
        for line in header.split("\r\n"):
            if line.lower().startswith("content-length:"):
                length = int(line.split(":", 1)[1].strip())
        if length is None:
            raise ValueError(f"no Content-Length in header: {header!r}")
        body_start = sep + 4
        body_end = body_start + length
        if len(self.buf) < body_end:
            return None
        body = bytes(self.buf[body_start:body_end])
        del self.buf[:body_end]
        return json.loads(body.decode("utf-8"))

    def read_message(self, timeout=READ_TIMEOUT):
        deadline = time.monotonic() + timeout
        with self.cond:
            while True:
                msg = self._try_extract()
                if msg is not None:
                    return msg
                if self.eof:
                    raise EOFError("subprocess closed stdout before a full message arrived")
                remaining = deadline - time.monotonic()
                if remaining <= 0:
                    raise TimeoutError(f"no message within {timeout}s")
                self.cond.wait(remaining)

    def read_response(self, expect_id, timeout=READ_TIMEOUT):
        deadline = time.monotonic() + timeout
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise TimeoutError(f"no response for id={expect_id} within {timeout}s")
            msg = self.read_message(timeout=remaining)
            if msg.get("id") == expect_id:
                return msg
            sys.stderr.write(f"smoke: skipping unsolicited message: {msg}\n")


def write_message(stream, obj):
    body = json.dumps(obj).encode("utf-8")
    stream.write(f"Content-Length: {len(body)}\r\n\r\n".encode("ascii"))
    stream.write(body)
    stream.flush()


def drain_stderr(proc):
    proc.kill()
    try:
        proc.wait(timeout=2)
    except subprocess.TimeoutExpired:
        pass
    try:
        return proc.stderr.read().decode(errors="replace")
    except Exception:
        return ""


def fail(proc, msg):
    err = drain_stderr(proc)
    sys.stderr.write(f"smoke check FAILED: {msg}\n")
    if err:
        sys.stderr.write(f"--- child stderr ---\n{err}\n")
    sys.exit(1)


def main():
    if len(sys.argv) != 2:
        sys.stderr.write("usage: smoke_check.py <path-to-sextant-binary>\n")
        sys.exit(2)
    binary = sys.argv[1]

    try:
        proc = subprocess.Popen(
            [binary],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
    except OSError as e:
        sys.stderr.write(f"smoke check FAILED to spawn {binary}: {e}\n")
        sys.exit(2)

    framer = Framer(proc)

    try:
        write_message(proc.stdin, {
            "jsonrpc": "2.0", "id": 1, "method": "initialize",
            "params": {"processId": None, "rootUri": None, "capabilities": {}},
        })
        resp = framer.read_response(1)
        name = resp.get("result", {}).get("serverInfo", {}).get("name")
        if name != "Sextant":
            fail(proc, f"initialize: expected serverInfo.name == 'Sextant', got {name!r}")

        write_message(proc.stdin, {
            "jsonrpc": "2.0", "method": "initialized", "params": {},
        })

        uri = "file:///tmp/smoke-check.lisp"
        write_message(proc.stdin, {
            "jsonrpc": "2.0", "method": "textDocument/didOpen",
            "params": {"textDocument": {
                "uri": uri, "languageId": "lisp", "version": 1,
                "text": "(mapcar #'identity '(1 2 3))\n",
            }},
        })

        write_message(proc.stdin, {
            "jsonrpc": "2.0", "id": 2, "method": "textDocument/hover",
            "params": {
                "textDocument": {"uri": uri},
                "position": {"line": 0, "character": 4},
            },
        })
        resp = framer.read_response(2)
        result = resp.get("result")
        value = (result or {}).get("contents", {}).get("value", "")
        if not value:
            fail(proc, f"hover: expected non-empty contents.value, got {result!r}")

        write_message(proc.stdin, {"jsonrpc": "2.0", "id": 3, "method": "shutdown"})
        framer.read_response(3)
        write_message(proc.stdin, {"jsonrpc": "2.0", "method": "exit"})

        try:
            proc.wait(timeout=SHUTDOWN_GRACE)
        except subprocess.TimeoutExpired:
            proc.kill()
            proc.wait()

        print("smoke check passed")
        sys.exit(0)

    except (TimeoutError, EOFError, ValueError, OSError) as e:
        err = drain_stderr(proc)
        sys.stderr.write(f"smoke check FAILED (infra): {e}\n")
        if err:
            sys.stderr.write(f"--- child stderr ---\n{err}\n")
        sys.exit(2)


if __name__ == "__main__":
    main()
