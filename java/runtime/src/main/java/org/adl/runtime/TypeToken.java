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

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof TypeToken)) {
      return false;
    }
    TypeToken other = (TypeToken) other0;
    return getJsonBinding().factory().typeExpr().equals(other.getJsonBinding().factory().typeExpr());
  }

  @Override
  public int hashCode() {
    return getJsonBinding().factory().typeExpr().hashCode();
  }
};
