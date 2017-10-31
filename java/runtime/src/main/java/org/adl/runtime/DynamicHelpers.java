package org.adl.runtime;

import org.adl.runtime.JsonBinding;
import org.adl.runtime.sys.dynamic.Dynamic;
import java.util.Optional;

public class DynamicHelpers
{
  /**
   * Wrap a value in a Dynamic.
   */
  public static <T> Dynamic toDynamic(JsonBinding<T> jsonBinding, T value) {
    return new Dynamic(jsonBinding.factory().typeExpr(), jsonBinding.toJson(value));
  }

  /**
   * Extract a value from a Dynamic
   */
  public static <T> Optional<T> fromDynamic(JsonBinding<T> jsonBinding, Dynamic dynamic) {
    if(dynamic.getTypeExpr().equals(jsonBinding.factory().typeExpr())) {
      return Optional.of(jsonBinding.fromJson(dynamic.getValue()));
    } else {
      return Optional.empty();
    }
  }

  /** Returns a Dynamic representing a null value. */
  public static Dynamic getNullInstance() {
    return DynamicHelpers.toDynamic(JsonBindings.VOID, null);
  }

};
