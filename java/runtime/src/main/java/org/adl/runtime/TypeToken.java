package org.adl.runtime;

import org.adl.runtime.JsonBinding;

public class TypeToken<T> {

  private JsonBinding<T> jsonBinding;

  public TypeToken(JsonBinding<T> jsonBinding) {
    this.jsonBinding = jsonBinding;
  }

  public JsonBinding<T> getJsonBinding() {
    return jsonBinding;
  }

  public static <T> JsonBinding<TypeToken<T>> jsonBinding(JsonBinding<T> jb) {
    return null;
  }
};
