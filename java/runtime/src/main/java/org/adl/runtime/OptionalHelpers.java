package org.adl.runtime;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

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

  
  public static <T> JsonBinding<Optional<T>> jsonBinding(final JsonBinding<T> bindingT) {
    final Factory<Optional<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<Optional<T>>() {
      public Factory<Optional<T>> factory() {
        return _factory;
      };

      public JsonElement toJson(Optional<T> value) {
        JsonObject result = new JsonObject();
        if (value.isPresent()) {
          result.add("just", bindingT.toJson(value.get()));
        } else {
          result.add("nothing", null);
        }
        return result;
      }

      public Optional<T> fromJson(JsonElement json) {
        JsonObject obj = json.getAsJsonObject();
        if (obj.has("just")) {
          return Optional.<T>of(bindingT.fromJson(obj.get("just")));
        } else {
          return Optional.<T>empty();
        }          
      }
    };
  }

};
