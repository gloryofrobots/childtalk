package ua.ho.gloryofrobots.childtalk.consoleapp;

import java.util.ArrayList;
import java.util.NoSuchElementException;
import java.util.Scanner;

import ua.ho.gloryofrobots.childtalk.bootstrap.BootstrapSuite;
import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.compilation.CompileSuite;
import ua.ho.gloryofrobots.childtalk.scheduler.EvalSuite;
import ua.ho.gloryofrobots.childtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;

public class ConsoleApplication {

    static class Options {
        boolean loadImage;
        String imagePath;

        boolean runFiles;
        ArrayList<String> filesToRun;
        boolean runSmallTalk;
        
        boolean showHelp;
    }

    private boolean mInitialised;

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
        builder.append(spaces+ "    ");
        builder.append("Eval line of code");
        builder.append(separator);

        builder.append("-r");
        builder.append(spaces+"         ");
        builder.append("Run Childtalk after loading files or evaluating code");
        builder.append(separator);

        builder.append("-f FILEPATH1, FILEPATH2");
        builder.append("                 ");

        builder.append("Load and eval files. For example: -f \"/usr/share/Planet.st\" \"/usr/share/Earth.st\"");
        builder.append(separator);

        builder.append("-h");
        builder.append(spaces+"         ");
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
            if (flag == 'v') {
                opts.showHelp = true;
            } else if (flag == 'f') {
                opts.runFiles = true;
                i = getArgValues(opts.filesToRun, i, args);
                if (i == -1) {
                    error("Error parsing files argument");
                }
            } else if (flag == 'i') {
                opts.loadImage = true;
                i++;
                opts.imagePath = args[i];
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
        }

        return -1;
    }

    public void run(String[] args) {
        Options opts = parseArguments(args);
        _run(opts);
    }

    public void _run(Options opts) {
        BootstrapSuite.setApplication(new Platform());
        printVersion();
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

        BootstrapSuite.bootstrap();

        if (opts.runFiles) {
            runFiles(opts.filesToRun);
        }

        if (opts.runSmallTalk) {
            runSmallTalk();
        }
    }

    private void print(String line) {
        System.out.print(line);
    }

    private void println(String line) {
        System.out.println(line);
    }

    private void runSmallTalk() {
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
            STObject result = EvalSuite.eval(line);
            /*if (result == ImageSuite.image().objects().NIL) {
                println("nil");
            } else {
                SchedulingSuite.callSelectorInNewProcess(ImageSuite.image()
                        .symbols().PRINTNL, result.getSTClass(), result);
            }*/
            SchedulingSuite.callSelectorInNewProcess(ImageSuite.image()
                    .symbols().PRINTNL, result.getSTClass(), result);
            // println(result.toString());

            return 1;
        } catch (NoSuchElementException e) {
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
            String[] arr = (String[]) filesToRun.toArray();
            EvalSuite.loadAndEvalFiles(arr);
        } catch (RuntimeException e) {
            e.printStackTrace();
            error("Error evaluate file %s", e.getMessage());
        }
    }

    private void loadDefaultImage() {
        String imagePath = "image/default.sim";
        String childTalkPath = System.getenv("CHILDTALK_PATH");
        if (childTalkPath != null) {
            imagePath = "/" + childTalkPath + "/" + imagePath;
        } else {
            String userPath = System.getProperty("user.dir");
            if (userPath != null) {
                imagePath = userPath + "/" + imagePath;
            }
        }

        loadImage(imagePath);
    }

    public void error(String format, Object... args) {
        String txt = String.format(format, args);
        System.err.println(txt);
        System.exit(-1);
    }

    private void loadImage(String imagePath) {
        boolean result = ImageSuite.loadImage(imagePath);
        // boolean result = ImageSuite.loadImageFromResource("ua/default.sim");
        if (!result) {
            error("Image loading error %s", imagePath);
        }

        mInitialised = true;
    }

    public static void main(String[] args) {
        ConsoleApplication app = new ConsoleApplication();
        String[] arguments = { "-r" };
        app.run(arguments);
    }
}
