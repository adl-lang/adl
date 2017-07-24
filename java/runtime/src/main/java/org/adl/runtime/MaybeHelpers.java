package org.adl.runtime;

import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonObject;

import java.util.Optional;

public class MaybeHelpers
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

  public static <T> Optional<T> nothing(@SuppressWarnings("unused") Void value) {
    return Optional.empty();
  }

  public static <T> Optional<T> just(T value) {
    return Optional.of(value);
  }


  public static <T> JsonBinding<Optional<T>> jsonBinding(final JsonBinding<T> bindingT) {
    final Factory<Optional<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<Optional<T>>() {
      @Override
      public Factory<Optional<T>> factory() {
        return _factory;
      };

      @Override
      public JsonElement toJson(Optional<T> value) {
        if (value.isPresent()) {
          JsonObject result = new JsonObject();
          result.add("just", bindingT.toJson(value.get()));
          return result;
        } else {
          return new JsonPrimitive("nothing");
        }
      }

      @Override
      public Optional<T> fromJson(JsonElement json) {
        String name = JsonBindings.unionNameFromJson(json);
        if (name.equals("nothing")) {
          return Optional.<T>empty();
        } else if (name.equals("just")) {
          try {
            return Optional.<T>of(JsonBindings.unionValueFromJson(json, bindingT));
          } catch (JsonParseException e) {
            e.pushField("just");
            throw e;
          }
        } else {
          throw new JsonParseException( "expected 'just' or {'nothing' : ...} for sys.types.Maybe" );
        }
      }
    };
  }

};
