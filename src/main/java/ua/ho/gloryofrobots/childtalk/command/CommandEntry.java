package ua.ho.gloryofrobots.childtalk.command;

import ua.ho.gloryofrobots.childtalk.command.CommandDispatcher.UnsupportedCommandException;

public class CommandEntry {
    public void execute(CommandDispatcher dispatcher) throws UnsupportedCommandException {
        dispatcher.onExecute(this);
    }
}
    
    
  