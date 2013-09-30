package ua.ho.gloryofrobots.childtalk.consoleapp;

import java.util.ArrayList;
import java.util.NoSuchElementException;
import java.util.Scanner;

import ua.ho.gloryofrobots.childtalk.bootstrap.BootstrapSuite;
import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.scheduler.EvalSuite;
import ua.ho.gloryofrobots.childtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;

public class ConsoleApplication {

    static class Options {
        boolean loadImage;
        String imagePath;

        boolean runFiles;
        ArrayList<String> filesToRun = new ArrayList<String>();

        boolean evalCode;
        String code;
        boolean runSmallTalk;

        boolean showHelp;
        public boolean printVersion;
    }

    private boolean mInitialised;
    private final String IMAGE_PATH_IN_RESOURCES = "/image/default.sim";

    private boolean isInitialised() {
        return mInitialised;
    }

    private void showHelp() {
        StringBuilder builder = new StringBuilder();
        String spaces = "                             ";
        String separator = System.getProperty("line.separator");

        builder.append("Usage:");
        builder.append(separator);

        builder.append("-i FILEPATH");
        builder.append(spaces);
        builder.append("Load image from FILEPATH");
        builder.append(separator);

        builder.append("-e CODE");
        builder.append(spaces + "    ");
        builder.append("Eval line of code");
        builder.append(separator);

        builder.append("-r");
        builder.append(spaces + "         ");
        builder.append("Run Childtalk after loading files or evaluating code");
        builder.append(separator);

        builder.append("-f FILEPATH1, FILEPATH2");
        builder.append("                 ");

        builder.append("Load and eval files. For example: -f \"/usr/share/Planet.st\" \"/usr/share/Earth.st\"");
        builder.append(separator);

        builder.append("-v");
        builder.append(spaces + "         ");
        builder.append("Print version");
        builder.append(separator);

        builder.append("-h");
        builder.append(spaces + "         ");
        builder.append("Print this page");
        builder.append(separator);

        print(builder.toString());
    }

    private void printVersion() {
        println("ChildTalk version 1.0");
    }

    public Options parseArguments(String[] args) {
        Options opts = new Options();
        if (args.length == 0) {
            opts.runSmallTalk = true;
            return opts;
        }

        int i = 0;
        while (i < args.length) {
            String arg = args[i];
            char check = arg.charAt(0);
            if (check != '-') {
                error("Error parsing arguments.");
            }
            char flag = arg.charAt(1);
            if (flag == 'h') {
                opts.showHelp = true;
            } else if (flag == 'v') {
                opts.printVersion = true;
            } else if (flag == 'f') {
                opts.runFiles = true;
                int j = getArgValues(opts.filesToRun, i + 1, args);
                if (j <= i) {
                    error("Error parsing files argument");
                }
                i = j;
            } else if (flag == 'i') {
                opts.loadImage = true;
                i++;
                opts.imagePath = args[i];
            } else if (flag == 'e') {
                opts.evalCode = true;
                i++;
                opts.code = args[i];
            } else if (flag == 'r') {
                opts.runSmallTalk = true;
            } else {
                error("Unknown argument -" + flag);
            }
            i++;
        }

        return opts;
    }

    private int getArgValues(ArrayList<String> dest, int i, String[] args) {
        while (i < args.length) {
            String arg = args[i];
            char check = arg.charAt(0);
            if (check == '-') {
                i--;
                return i;
            }
            dest.add(arg);
            i++;
        }

        return i;
    }

    public void run(String[] args) {
        Options opts = parseArguments(args);
        _run(opts);
    }

    public void _run(Options opts) {
        BootstrapSuite.setApplication(new Platform());
        printVersion();
        if (opts.printVersion) {
            return;
        }

        if (opts.showHelp) {
            showHelp();
            return;
        }

        if (opts.loadImage) {
            loadImage(opts.imagePath);
        }

        if (!isInitialised()) {
            loadDefaultImage();
        }

        if (opts.runFiles) {
            runFiles(opts.filesToRun);
        }
        
        if (opts.evalCode) {
            evalCode(opts.code);
        }
        
        if (opts.runSmallTalk) {
            runSmallTalk();
        }
    }

    private void evalCode(String code) {
        STObject result = EvalSuite.eval(code);
        SchedulingSuite.callSelectorInNewProcess(
                ImageSuite.image().symbols().PRINTNL, result.getSTClass(),
                result);
    }

    private void print(String line) {
        System.out.print(line);
    }

    private void println(String line) {
        System.out.println(line);
    }

    private void runSmallTalk() {
        println("");
        Scanner scanner = new Scanner(System.in);
        while (true) {
            int status = evalLine(scanner);

            if (status <= 0) {
                System.exit(status);
            }
        }
    }

    private int evalLine(Scanner scanner) {
        print("> ");
        try {
            String line = scanner.nextLine();
            evalCode(line);
            // println(result.toString());

            return 1;
        } catch (NoSuchElementException e) {
            println("");
            return 0;
        } catch (RuntimeException e) {
            String error = e.getMessage();
            System.err.println(error);
            println("");
            // e.printStackTrace();
            return 1;
        }
    }

    private void runFiles(ArrayList<String> filesToRun) {
        try {
            String[] arr = new String[filesToRun.size()];
            int i = 0;
            for (Object obj : filesToRun) {
                arr[i] = ((String) obj).replace("\"", "");
                i++;
            }
            EvalSuite.loadAndEvalFiles(arr);
        } catch (RuntimeException e) {
            // e.printStackTrace();
            error("Error evaluate file %s", e.getMessage());
        }
    }

    private void loadDefaultImage() {
        boolean result = ImageSuite
                .loadImageFromResource(IMAGE_PATH_IN_RESOURCES);
        
        if (!result) {
            error("Error loading default image as resource at %s",
                    IMAGE_PATH_IN_RESOURCES);
        }
        
        mInitialised = true;
    }

    public void error(String format, Object... args) {
        String txt = String.format(format, args);
        System.err.println(txt);
        System.exit(-1);
    }

    private void loadImage(String imagePath) {
        boolean result = ImageSuite.loadImage(imagePath);
        if (!result) {
            error("Image loading error %s", imagePath);
        }

        mInitialised = true;
    }

    public static void main(String[] args) {
        ConsoleApplication app = new ConsoleApplication();
        /*String[] arguments = {
                
                "-f",
                "\"/home/gloryofrobots/develop/smalltalk/childtalk/st/tests/old/expression_test.st\"",
                "/home/gloryofrobots/develop/smalltalk/childtalk/st/tests/old/expression_test2.st",
                "-e", "SmallTalk saveImage:'/home/gloryofrobots/develop/smalltalk/childtalk/image.sim'", "-r"};
    */
        //String[] arguments = {"-i","/home/gloryofrobots/develop/smalltalk/childtalk/image.sim", "-r"};
        
        //SmallTalk saveImage:'/home/gloryofrobots/develop/smalltalk/childtalk/image.sim'
        app.run(args);
    }
}
