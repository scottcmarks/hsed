/*
 * hsed_test_signals.c: what hsed-test reads of a signal's action that System.Posix.Signals does not give back
 * (Hsed.Jig.RunSpec; the M4c review's verification): whether its handler is reset to the default once delivered
 * (SA_RESETHAND, the RTS's STG_SIG_RST, which System.Posix.Signals' CatchOnce asks for and GHC's own SIGINT
 * handler has, so that a second signal kills) or stays. For the tests alone; no module of the library calls it.
 */
#include <signal.h>
#include <stddef.h>

/* 1: a handler reset once delivered, where sigaction reports SA_RESETHAND (Linux; macOS does not, though it
   resets: a test sees the reset there by delivering the signal once, which leaves the default action); 0: a
   handler that stays, or one whose reset is not reported; -1: the default action, or ignored; -2: the action
   cannot be read. */
int hsed_test_signal_resets(int sig)
{
    struct sigaction now;
    if (sigaction(sig, NULL, &now) != 0)
        return -2;
    if (!(now.sa_flags & SA_SIGINFO) && (now.sa_handler == SIG_DFL || now.sa_handler == SIG_IGN))
        return -1;
    return (now.sa_flags & SA_RESETHAND) ? 1 : 0;
}
