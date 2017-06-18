package org.adl.runtime;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import java.io.FileReader;
import java.io.IOException;

/**
 * Simple functions to reduce boilerplate in working with json for ADL values
 */
public class JsonHelpers
{
  static Gson gson = new GsonBuilder()
    .serializeNulls()
    .create();

  static Gson lenientGson = new GsonBuilder()
    .serializeNulls()
    .setLenient()
    .create();

  /**
   * Read an ADL value from a json file
   */
  public static <T> T fromFile(JsonBinding<T> binding, String path) throws IOException {
    return binding.fromJson(gson.fromJson(new FileReader(path), JsonElement.class));
  }

  /**
   * Read an ADL value from a leniently formatted json file.
   *
   * (See `setLenient` at
   *  https://static.javadoc.io/com.google.code.gson/gson/2.6.2/com/google/gson/stream/JsonReader.html)
   */
  public static <T> T fromLenientFile(JsonBinding<T> binding, String path) throws IOException {
    return binding.fromJson(lenientGson.fromJson(new FileReader(path), JsonElement.class));
  }

  /**
   * Parse json element from a string
   */
  public static JsonElement jsonFromString(String s) {
    return gson.fromJson(s, JsonElement.class);
  }
};
