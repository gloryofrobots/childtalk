package ua.ho.gloryofrobots.childtalk.bytecode;

import ua.ho.gloryofrobots.childtalk.inout.SignalSuite;

public class BytecodeUtils {
    static final int BYTECODE_BIT_COUNT = 32;
    static final int COMMAND_BIT_COUNT = 6;
    static final int ARGUMENT_BIT_COUNT = BYTECODE_BIT_COUNT
            - COMMAND_BIT_COUNT;

    static final int BYTECODE_MAX_VALUE = (1 << BYTECODE_BIT_COUNT) - 1;
    static final int COMMAND_MAX_VALUE = (1 << COMMAND_BIT_COUNT) - 1;
    static final int ARGUMENT_MAX_VALUE = (1 << ARGUMENT_BIT_COUNT) - 1;

    static final int COMMAND_MASK = COMMAND_MAX_VALUE << ARGUMENT_BIT_COUNT;
    static final int ARGUMENT_MASK = ARGUMENT_MAX_VALUE;

    static int pack(short command, int argument) {
        if (argument > ARGUMENT_MAX_VALUE) {
            SignalSuite
                    .error("Bytecode pack error : argument value %d too large, max supported value %d",
                            argument, ARGUMENT_MAX_VALUE);
        }

        if (command > COMMAND_MAX_VALUE) {
            SignalSuite
                    .error("Bytecode pack error : command value %d too large, max supported value %d",
                            command, COMMAND_MAX_VALUE);
        }

        int code = (command << ARGUMENT_BIT_COUNT) + argument;
        return code;
    }

    static short unpackCommand(int code) {
        short command = (short) ((code & COMMAND_MASK) >>> ARGUMENT_BIT_COUNT);
        return command;
    }

    static int unpackArgument(int code) {
        int argument = code & ARGUMENT_MASK;
        return argument;
    }

    static void printBinary(String label, int num) {
        System.out.println(label + ": " + Integer.toBinaryString(num));
    }
}
