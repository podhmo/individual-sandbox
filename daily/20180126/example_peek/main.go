package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
	"syscall"

	"github.com/pkg/errors"
)

func run() error {
	pidStr := os.Getenv("TARGET_PID")
	if pidStr == "" {
		return errors.New("please set TARGET_PID")
	}
	pid, err := strconv.Atoi(pidStr)
	if err != nil {
		return errors.Wrap(err, "atoi")
	}

	if err := syscall.PtraceAttach(pid); err != nil {
		return errors.Wrap(err, "attach")
	}
	syscall.PtraceSetOptions(pid, syscall.PTRACE_O_TRACESYSGOOD)

	var s syscall.WaitStatus
	rusage := syscall.Rusage{}
	var regs syscall.PtraceRegs
	var prevRax uint64

	isEnterStop := false
	for {
		_, err := syscall.Wait4(pid, &s, syscall.WALL, &rusage)
		if err != nil {
			return errors.Wrap(err, "wait4")
		}
		if s.Exited() {
			break
		}

		syscall.PtraceGetRegs(pid, &regs)
		switch regs.Orig_rax {
		case syscall.SYS_WRITE:
			if isEnterStop {
				body := string(peekBytes(pid, int(regs.Rsi), int(regs.Rdx)))
				fmt.Println(strings.Replace(body, "\n", "", -1))
				isEnterStop = !isEnterStop
			}
		case prevRax:
			isEnterStop = !isEnterStop
		}
		prevRax = regs.Orig_rax
		var signal int
		syscall.PtraceSyscall(pid, signal)
	}

	if err := syscall.PtraceDetach(pid); err != nil {
		return errors.Wrap(err, "detach")
	}
	return nil
}

func peekBytes(pid, addr, size int) []byte {
	buf := make([]byte, (size+7)/8)
	for i := range buf {
		syscall.PtracePeekData(pid, uintptr(addr+8*i), buf)
	}
	fmt.Println(buf)
	return buf
}

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}
