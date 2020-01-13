#define _POSIX_C_SOURCE 200809L
#include "utils.h"
#include "vm.h"

void cli_help(const char* argv[]) {
    printf("Usage: %s dump|run|repl [filename]\n", argv[0]);
    exit(0);
}

int main(int argc, const char* argv[]) {
    if (argc == 2 && strcmp(argv[1], "repl") == 0)
        vm_repl();
    else if (argc == 3) {
        char* source = NULL;
        size_t source_len = 0;
        if (strcmp(argv[2], "-") == 0)
            read_stdin(&source, &source_len);
        else
            read_file(argv[2], &source, &source_len);

        if (strcmp(argv[1], "dump") == 0)
            vm_interpret(source, source_len, vm_dump);
        else if (strcmp(argv[1], "run") == 0)
            return vm_interpret(source, source_len, vm_run_bytecode);
    } else
        cli_help(argv);
}
