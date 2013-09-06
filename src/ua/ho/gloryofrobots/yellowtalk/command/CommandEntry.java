package ua.ho.gloryofrobots.yellowtalk.command;

import ua.ho.gloryofrobots.yellowtalk.command.CommandDispatcher.UnsupportedCommandException;

public class CommandEntry {
    public void execute(CommandDispatcher dispatcher) throws UnsupportedCommandException {
        dispatcher.onExecute(this);
    }
}
    
    
  