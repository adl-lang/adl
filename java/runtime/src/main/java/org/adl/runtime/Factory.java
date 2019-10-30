package org.adl.runtime;

import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.JsonBinding;

/**
 * A generic factory class for constructing generic instances.
 *
 * @param <T> The type of object being constructed.
 */
public interface Factory<T> {
  /**
   * Create a value of type T, initialised to a default value.
   */
  T create();

  /**
   * Create a copy of a value of type T. Mutable values will be deep
   * copied.
   */
  T create(T other);

  /**
   * Return a type expression describing the type of T
   */
  TypeExpr typeExpr();

  /**
   * Return a jsonbinding for T
   */
  JsonBinding<T> jsonBinding();
};
