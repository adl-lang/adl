package org.adl.runtime;

import com.google.gson.JsonElement;

/**
 *  Interface for GSON based serialisation.
 */
public interface JsonBinding<T>
{
  Factory<T> factory();
  JsonElement toJson(T value);
  T fromJson(JsonElement json);

  default String toJsonString(T value) {
    return JsonHelpers.gson.toJson(toJson(value));
  }

  default T fromJsonString(String str) {
    return fromJson(JsonHelpers.gson.fromJson(str, JsonElement.class));
  }
};
