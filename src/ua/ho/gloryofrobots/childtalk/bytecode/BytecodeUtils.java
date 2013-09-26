package ua.ho.gloryofrobots.childtalk.bytecode;

import ua.ho.gloryofrobots.childtalk.Universe;
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
        // BytecodeUtils.printBinary("code", code);
        // BytecodeUtils.printBinary("COMMAND_MASK", COMMAND_MASK);
        // BytecodeUtils.printBinary("ARGUMENT_BIT_COUNT",ARGUMENT_BIT_COUNT);
        //
        // BytecodeUtils.printBinary("code & COMMAND_MASK", code &
        // COMMAND_MASK);
        // BytecodeUtils.printBinary("(code & COMMAND_MASK) >> ARGUMENT_BIT_COUNT",
        // (code & COMMAND_MASK) >> ARGUMENT_BIT_COUNT);
        // BytecodeUtils.printBinary("(code & COMMAND_MASK) >>> ARGUMENT_BIT_COUNT",
        // (code & COMMAND_MASK) >>> ARGUMENT_BIT_COUNT);
        short command = (short) ((code & COMMAND_MASK) >>> ARGUMENT_BIT_COUNT);
        return command;
    }

    static int unpackArgument(int code) {
        int argument = code & ARGUMENT_MASK;
        return argument;
    }

    static void printBinary(String label, int num) {
        System.out.println(label + ": " + Integer.toBinaryString(num));
        /*
         * char [] str = new char[33]; int i; for (i=31; i>=0; i--) { if
         * ((num&1) == 0) { str[i] = '0'; } else { str[i] = '1'; } num >>= 1; }
         * 
         * System.out.println(str);
         */
    }

    public static void main(String[] args) {
        /*
         * int argument = 3; short command = 32; int code =
         * BytecodeUtils.pack(command, argument); int argument_r =
         * BytecodeUtils.unpackArgument(code); short command_r =
         * BytecodeUtils.unpackCommand(code);
         * System.out.printf("%d %d %d %d",command,argument, command_r,
         * argument_r);
         */
          /*int argument = 3; short command = 32;
          BytecodeUtils.printBinary("argument", argument);
          BytecodeUtils.printBinary("command", command);
          
          BytecodeUtils.printBinary("command << SYX_BYTECODE_ARGUMENT_BITS",
          command << ARGUMENT_BIT_COUNT); BytecodeUtils.printBinary(
          "command << SYX_BYTECODE_ARGUMENT_BITS + argument", (command <<
          ARGUMENT_BIT_COUNT) + argument);
          
          int code = ((command << ARGUMENT_BIT_COUNT) + argument);
          
          BytecodeUtils.printBinary("SYX_BYTECODE_ARGUMENT_MASK",
          ARGUMENT_MASK); BytecodeUtils.printBinary("code", code);
          BytecodeUtils.printBinary(" byte & SYX_BYTECODE_ARGUMENT_MASK", code
          & ARGUMENT_MASK);
          */
          /*
          int a = 2;  int b = 3;
          int c = b; //c |= b;
          BytecodeUtils.printBinary("a", a);
          BytecodeUtils.printBinary("b", b);
          BytecodeUtils.printBinary("c", c);
          int d = c & b;
          
          if((c & b) != 0) {
              System.out.println("c & b");
          }
          if((c & a) != 0) {
              System.out.println("c & a");
          }*/
    }
}
