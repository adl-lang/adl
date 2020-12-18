package org.adl.runtime;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;

import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import org.adl.runtime.TypeToken;

import java.util.ArrayList;
import java.util.List;
import java.util.Base64;
import java.util.HashMap;
import java.util.Optional;
import java.util.Map;
import java.util.Set;

/**
 *  helper implementations for GSON based serialisation.
 */
public class JsonBindings
{
  public static final JsonBinding<Void> VOID = new JsonBinding<Void>() {

    @Override
    public Factory<Void> factory() {
      return Factories.VOID;
    }

    @Override
    public JsonElement toJson(Void value) {
      return JsonNull.INSTANCE;
    }

    @Override
    public Void fromJson(JsonElement json) {
      if (json.isJsonNull()) {
        return null;
      }
      throw new JsonParseException("expected null");
    }
  };

  public static final JsonBinding<Boolean> BOOLEAN = new JsonBinding<Boolean>() {

    @Override
    public Factory<Boolean> factory() {
      return Factories.BOOLEAN;
    }

    @Override
    public JsonElement toJson(Boolean value) {
      return new JsonPrimitive(value);
    }

    @Override
    public Boolean fromJson(JsonElement json) {
      try {
        return json.getAsBoolean();
      } catch (UnsupportedOperationException | ClassCastException e) {
        throw new JsonParseException("expected a boolean");
      }
    }
  };

  public static final JsonBinding<Byte> INT8 = new JsonBinding<Byte>() {

    @Override
    public Factory<Byte> factory() {
      return Factories.INT8;
    }

    @Override
    public JsonElement toJson(Byte value) {
      return new JsonPrimitive(value);
    }

    @Override
    public Byte fromJson(JsonElement json) {
      try {
        return json.getAsNumber().byteValue();
      } catch (UnsupportedOperationException | ClassCastException e) {
        throw new JsonParseException("expected a number");
      }
    }
  };

  public static final JsonBinding<Short> INT16 = new JsonBinding<Short>() {

    @Override
    public Factory<Short> factory() {
      return Factories.INT16;
    }

    @Override
    public JsonElement toJson(Short value) {
      return new JsonPrimitive(value);
    }

    @Override
    public Short fromJson(JsonElement json) {
      try {
        return json.getAsNumber().shortValue();
      } catch (UnsupportedOperationException | ClassCastException | NumberFormatException e) {
        throw new JsonParseException("expected a number");
      }
    }
  };

  public static final JsonBinding<Integer> INT32 = new JsonBinding<Integer>() {

    @Override
    public Factory<Integer> factory() {
      return Factories.INT32;
    }

    @Override
    public JsonElement toJson(Integer value) {
      return new JsonPrimitive(value);
    }

    @Override
    public Integer fromJson(JsonElement json) {
      try {
        return json.getAsNumber().intValue();
      } catch (UnsupportedOperationException | ClassCastException | NumberFormatException e) {
        throw new JsonParseException("expected a number");
      }
    }
  };

  public static final JsonBinding<Long> INT64 = new JsonBinding<Long>() {

    @Override
    public Factory<Long> factory() {
      return Factories.INT64;
    }

    @Override
    public JsonElement toJson(Long value) {
      return new JsonPrimitive(value);
    }

    @Override
    public Long fromJson(JsonElement json) {
      try {
        return json.getAsNumber().longValue();
      } catch (UnsupportedOperationException | ClassCastException | NumberFormatException e) {
        throw new JsonParseException("expected a number");
      }
    }
  };

  public static final JsonBinding<Byte> WORD8 = new JsonBinding<Byte>() {

    @Override
    public Factory<Byte> factory() {
      return Factories.WORD8;
    }

    @Override
    public JsonElement toJson(Byte value) {
      return new JsonPrimitive(value);
    }

    @Override
    public Byte fromJson(JsonElement json) {
      try {
        return json.getAsNumber().byteValue();
      } catch (UnsupportedOperationException | ClassCastException e) {
        throw new JsonParseException("expected a number");
      }
    }
  };

  public static final JsonBinding<Short> WORD16 = new JsonBinding<Short>() {

    @Override
    public Factory<Short> factory() {
      return Factories.WORD16;
    }

    @Override
    public JsonElement toJson(Short value) {
      return new JsonPrimitive(value);
    }

    @Override
    public Short fromJson(JsonElement json) {
      try {
        return json.getAsNumber().shortValue();
      } catch (UnsupportedOperationException | ClassCastException | NumberFormatException e) {
        throw new JsonParseException("expected a number");
      }
    }
  };

  public static final JsonBinding<Integer> WORD32 = new JsonBinding<Integer>() {

    @Override
    public Factory<Integer> factory() {
      return Factories.WORD32;
    }

    @Override
    public JsonElement toJson(Integer value) {
      return new JsonPrimitive(value);
    }

    @Override
    public Integer fromJson(JsonElement json) {
      try {
        return json.getAsNumber().intValue();
      } catch (UnsupportedOperationException | ClassCastException | NumberFormatException e) {
        throw new JsonParseException("expected a number");
      }
    }
  };

  public static final JsonBinding<Long> WORD64 = new JsonBinding<Long>() {

    @Override
    public Factory<Long> factory() {
      return Factories.WORD64;
    }

    @Override
    public JsonElement toJson(Long value) {
      return new JsonPrimitive(value);
    }

    @Override
    public Long fromJson(JsonElement json) {
      try {
        return json.getAsNumber().longValue();
      } catch (UnsupportedOperationException | ClassCastException | NumberFormatException e) {
        throw new JsonParseException("expected a number");
      }
    }
  };

  public static final JsonBinding<Float> FLOAT = new JsonBinding<Float>() {

    @Override
    public Factory<Float> factory() {
      return Factories.FLOAT;
    }

    @Override
    public JsonElement toJson(Float value) {
      return new JsonPrimitive(value);
    }

    @Override
    public Float fromJson(JsonElement json) {
      try {
        return json.getAsNumber().floatValue();
      } catch (UnsupportedOperationException | ClassCastException | NumberFormatException e) {
        throw new JsonParseException("expected a number");
      }
    }
  };

  public static final JsonBinding<Double> DOUBLE = new JsonBinding<Double>() {

    @Override
    public Factory<Double> factory() {
      return Factories.DOUBLE;
    }

    @Override
    public JsonElement toJson(Double value) {
      return new JsonPrimitive(value);
    }

    @Override
    public Double fromJson(JsonElement json) {
      try {
        return json.getAsNumber().doubleValue();
      } catch (UnsupportedOperationException | ClassCastException | NumberFormatException e) {
        throw new JsonParseException("expected a number");
      }
    }
  };

  public static final JsonBinding<String> STRING = new JsonBinding<String>() {

    @Override
    public Factory<String> factory() {
      return Factories.STRING;
    }

    @Override
    public JsonElement toJson(String value) {
      return new JsonPrimitive(value);
    }

    @Override
    public String fromJson(JsonElement json) {
      if (json.isJsonPrimitive() && json.getAsJsonPrimitive().isString()) {
        return json.getAsString();
      }

      throw new JsonParseException("expected a string");
    }
  };

  // The JSON factory class lives here (and not in Factories.java) as we
  // don't want to force a dependency on gson if json serialization is not
  // used

  public static final Factory<JsonElement> JSON_FACTORY = new Factory<JsonElement>() {
    @Override
    public JsonElement create() { return JsonNull.INSTANCE; }
    @Override
    public JsonElement create(JsonElement other) { return other; }
    @Override
    public TypeExpr typeExpr() { return new TypeExpr(TypeRef.primitive("Json"), new ArrayList<>()); }
    @Override
    public JsonBinding<JsonElement> jsonBinding() { return JSON; }
  };

  public static final JsonBinding<JsonElement> JSON = new JsonBinding<JsonElement>() {

    @Override
    public Factory<JsonElement> factory() {
      return JSON_FACTORY;
    }

    @Override
    public JsonElement toJson(JsonElement value) {
      return value;
    }

    @Override
    public JsonElement fromJson(JsonElement json) {
      return json;
    }
  };

  public static final JsonBinding<ByteArray> BYTE_ARRAY = new JsonBinding<ByteArray>() {

    @Override
    public Factory<ByteArray> factory() {
      return Factories.BYTE_ARRAY;
    }

    @Override
    public JsonElement toJson(ByteArray value) {
      return new JsonPrimitive(new String(Base64.getEncoder().encode(value.getValue())));
    }

    @Override
    public ByteArray fromJson(JsonElement json) {
      try {
        return new ByteArray(Base64.getDecoder().decode(json.getAsString().getBytes()));
      } catch (UnsupportedOperationException | ClassCastException e) {
        throw new JsonParseException("expected a base 64 encoded string");
      } catch (RuntimeException e) {
        throw new JsonParseException("expected a base 64 encoded string");
      }
    }
  };

  public static <T> JsonBinding<List<T>> list(final JsonBinding<T> factoryT) {
    return new JsonBinding<List<T>>() {

      @Override
      public Factory<List<T>> factory() {
        return Factories.list(factoryT.factory());
      }

      @Override
      public JsonElement toJson(List<T> value) {
        JsonArray result = new JsonArray();
        for (T v : value) {
          result.add(factoryT.toJson(v));
        }
        return result;
      }

      @Override
      public List<T> fromJson(JsonElement json) {
        List<T> result = new ArrayList<>();
        JsonArray jarray;
        try {
          jarray = json.getAsJsonArray();
        } catch (UnsupportedOperationException | ClassCastException e) {
          throw new JsonParseException("expected an array");
        }
        int i = 0;
        for (JsonElement je : jarray) {
          try {
            result.add(factoryT.fromJson(je));
          } catch (JsonParseException e) {
            e.pushIndex(i);
            throw e;
          }
          i++;
        }
        return result;
      }
    };
  }

  public static <T> JsonBinding<Map<String,T>> stringMap(final JsonBinding<T> factoryT) {
    return new JsonBinding<Map<String,T>>() {

      @Override
      public Factory<Map<String,T>> factory() {
        return Factories.stringMap(factoryT.factory());
      }

      @Override
      public JsonElement toJson(Map<String,T> value) {
        JsonObject result = new JsonObject();
        for (Map.Entry<String,T> e : value.entrySet()) {
          result.add(e.getKey(), factoryT.toJson(e.getValue()));
        }
        return result;
      }

      @Override
      public Map<String,T> fromJson(JsonElement json) {
        JsonObject jobj;
        try {
          jobj = json.getAsJsonObject();
        } catch (UnsupportedOperationException | ClassCastException e) {
          throw new JsonParseException("expected an object");
        }
        Map<String,T> result = new HashMap<>();
        for (Map.Entry<String,JsonElement> e : jobj.entrySet()) {
          try {
            result.put(e.getKey(), factoryT.fromJson(e.getValue()));
          } catch (JsonParseException jpe) {
            jpe.pushField(e.getKey());
            throw jpe;
          }
        }
        return result;
      }
    };
  }

  public static <T> JsonBinding<Optional<T>> nullable(final JsonBinding<T> factoryT) {
    return new JsonBinding<Optional<T>>() {

      @Override
      public Factory<Optional<T>> factory() {
        return Factories.nullable(factoryT.factory());
      }

      @Override
      public JsonElement toJson(Optional<T> value) {
        if (value.isPresent()) {
          return factoryT.toJson(value.get());
        } else {
          return JsonNull.INSTANCE;
        }
      }

      @Override
      public Optional<T> fromJson(JsonElement json) {
        if (json.isJsonNull()) {
          return Optional.<T>empty();
        } else {
          return Optional.<T>of(factoryT.fromJson(json));
        }
      }
    };
  }

  public static <T> JsonBinding<TypeToken<T>> typeProxy(final JsonBinding<T> factoryT) {
    return TypeToken.jsonBinding(factoryT);
  }


  public static <T> JsonElement unionToJson(String name, T value, JsonBinding<T> jsonBinding) {
    if(value == null) {
      return new JsonPrimitive(name);
    } else {
      JsonObject result = new JsonObject();
      result.add(name, jsonBinding.toJson(value));
      return result;
    }
  }

  public static String unionNameFromJson(JsonElement json) {
    if(json.isJsonObject()) {
      JsonObject jsonObject = json.getAsJsonObject();
      Set<Map.Entry<String,JsonElement>> entries = jsonObject.entrySet();
      if (entries.size() != 1) {
        // If the ADL is the result of a JSON merge, use the most
        // recently added field
        if (jsonObject.has(LAST_MERGED_FIELD)) {
          return jsonObject.getAsJsonPrimitive(LAST_MERGED_FIELD).getAsString();
        }
        throw new JsonParseException("union object must have a single field");
      }
      for (Map.Entry<String,JsonElement> v : entries) {
        return v.getKey();
      }
      throw new IllegalStateException();
    } else {
      try {
        return json.getAsString();
      } catch (UnsupportedOperationException | ClassCastException e) {
        throw new JsonParseException( "expected an object or string");
      }
    }
  }

  public static <T> T unionValueFromJson(JsonElement json, JsonBinding<T> jsonBinding) {
    if(json.isJsonObject()) {
      String field = unionNameFromJson(json);
      JsonElement jv = json.getAsJsonObject().get(field);
      try {
        return jsonBinding.fromJson(jv);
      } catch (JsonParseException e) {
        e.pushField(field);
        throw e;
      }
    } else {
      return null;
    }
  }

  public static <T> JsonElement unionToItJson(String tag, String name, T value, JsonBinding<T> jsonBinding) {
    JsonObject jv = jsonBinding.toJson(value).getAsJsonObject();
    jv.addProperty(tag, name);
    return jv;
  }

  public static String unionNameFromItJson(String tag, JsonElement json) {
    return json.getAsJsonObject().get(tag).getAsString();
  }

  public static <T> T unionValueFromItJson(String tag, JsonElement json, JsonBinding<T> jsonBinding) {
    return jsonBinding.fromJson(json);
  }

  public static JsonObject objectFromJson(JsonElement json) {
    if (!json.isJsonObject()) {
      throw new JsonParseException("expected an object");
    }
    return json.getAsJsonObject();
  }

  public static <T> T fieldFromJson(JsonObject obj, String field, JsonBinding<T> jsonBinding) {
    JsonElement el = objectFromJson(obj).get(field);
    if (el == null) {
      throw new JsonParseException( "expected field " + field);
    }
    try {
      return jsonBinding.fromJson(el);
    } catch(JsonParseException e) {
      e.pushField(field);
      throw e;
    }
  }

  public static String LAST_MERGED_FIELD = "_adl_last_merged_field";
};
