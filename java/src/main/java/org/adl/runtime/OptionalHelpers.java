package org.adl.runtime;

import java.util.Optional;

public class OptionalHelpers
{
  public static <T> Factory<Optional<T>> factory(final Factory<T> tFactory) {
    return new Factory<Optional<T>>() {
      @Override
      public Optional<T> create() {
        return Optional.empty();
      }

      @Override
      public Optional<T> create(Optional<T> other) {
        return other.map( tFactory::create );
      }
    };
  }

  public static <T> Optional<T> nothing(Void value) {
    return Optional.empty();
  }

  public static <T> Optional<T> just(T value) {
    return Optional.of(value);
  }
};
