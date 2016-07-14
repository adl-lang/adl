package org.adl.runtime;

public interface Factory<T>
{
  /**
   * Create a value of type T, initialised to a default value
   */
  T create();

  /**
   * Create a copy of a value of type T. Mutable values will be deep
   * copied.
   */
  T create(T other);
};
