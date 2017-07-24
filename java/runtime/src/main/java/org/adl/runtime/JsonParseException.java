package org.adl.runtime;

import java.util.ArrayList;
import java.util.Collections;

@SuppressWarnings("serial")
public class JsonParseException extends com.google.gson.JsonParseException
{
  private String message;
  private ArrayList<String> context;

  public JsonParseException(String message) {
    super(message);
    this.message = message;
    this.context = new ArrayList<>();
  }

  @Override
  public String getMessage() {
    return this.message + " at " + contextString();
  }

  public void pushField(String fieldName) {
    context.add( fieldName );
  }

  public void pushIndex(int index) {
    context.add("[" + index + "]");
  }

  private String contextString() {
    ArrayList<String> rcontext = new ArrayList<>(context);
    rcontext.add("$");
    Collections.reverse(rcontext);
    return String.join(".", rcontext);
  }
}
