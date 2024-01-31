package org.adl.runtime;

import static org.adl.runtime.JsonBindings.LAST_MERGED_FIELD;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.nio.charset.Charset;

import java.util.Map;
import java.util.Objects;

/**
 * Simple functions to reduce boilerplate in working with json for ADL values
 */
public class JsonHelpers {

  static Gson gson = new GsonBuilder()
    .serializeNulls()
    .create();

  static Gson lenientGson = new GsonBuilder()
    .serializeNulls()
    .setLenient()
    .create();

  /**
   * Converts an ADL value to a string
   */
  public static <T> String toString(JsonBinding<T> binding, T value) {
    return gson.toJson(binding.toJson(value));
  }

  /**
   * Converts an ADL value to a string, using the specified gson serializer
   */
  public static <T> String toString(JsonBinding<T> binding, Gson gson, T value) {
    return gson.toJson(binding.toJson(value));
  }

  /**
   *  Parses an ADL value from a string
   */
  public static <T> T fromString(JsonBinding<T> binding, String value) {
    return binding.fromJson(gson.fromJson(value, JsonElement.class));
  }

  /**
   *  Parses an ADL value from a leniently formatted json string.
   *
   * (See `setLenient` at
   *  https://static.javadoc.io/com.google.code.gson/gson/2.6.2/com/google/gson/stream/JsonReader.html)
   */
  public static <T> T fromLenientString(JsonBinding<T> binding, String value) {
    return binding.fromJson(lenientGson.fromJson(value, JsonElement.class));
  }

  /**
   * Reads an ADL value from a UTF-8 encoded json file
   */
  public static <T> T fromFile(JsonBinding<T> binding, String path) throws IOException {
    InputStreamReader utf8Reader = getUtf8Reader(path);
    try {
      return binding.fromJson(gson.fromJson(utf8Reader, JsonElement.class));
    } finally {
      utf8Reader.close();
    }
  }

  /**
   * Gets the InputStreamReader for a file using the UTF-8 charset
   */
  private static InputStreamReader getUtf8Reader(String path) throws FileNotFoundException {
    return new InputStreamReader(new FileInputStream(path), Charset.forName("UTF-8"));
  }

  /**
   * Gets the InputStreamReader for a URL resource using the UTF-8 charset
   */
  private static InputStreamReader getUtf8Reader(URL file) throws IOException {
    return new InputStreamReader(file.openStream(), Charset.forName("UTF-8"));
  }

  /**
   * Writes an ADL value to a UTF-8 json file
   */
  public static <T> void toFile(JsonBinding<T> binding, T value, String path) throws IOException {
    toFile(binding, gson, value, path);
  }

  /**
   * Writes an ADL value to a UTF-8 json file, using the specified gson serializer
   */
  public static <T> void toFile(JsonBinding<T> binding, Gson gson, T value, String path) throws IOException {
    OutputStreamWriter writer = new OutputStreamWriter(new FileOutputStream(path), Charset.forName("UTF-8"));
    try {
      gson.toJson( binding.toJson(value), writer );
    } finally {
      writer.close();
    }
  }

  /**
   * Reads an ADL value from a leniently formatted UTF-8 json file.
   *
   * (See `setLenient` at
   *  https://static.javadoc.io/com.google.code.gson/gson/2.6.2/com/google/gson/stream/JsonReader.html)
   */
  public static <T> T fromLenientFile(JsonBinding<T> binding, String path) throws IOException {
    try (InputStreamReader utf8Reader = getUtf8Reader(path)) {
      return binding.fromJson(lenientGson.fromJson(utf8Reader, JsonElement.class));
    }
  }

  /**
   * Reads an ADL value from a leniently formatted UTF-8 json resource.
   *
   * (See `setLenient` at
   *  https://static.javadoc.io/com.google.code.gson/gson/2.6.2/com/google/gson/stream/JsonReader.html)
   */
  public static <T> T fromLenientUrl(JsonBinding<T> binding, URL file) throws IOException {
    try (InputStreamReader utf8Reader = getUtf8Reader(file)) {
      return binding.fromJson(lenientGson.fromJson(utf8Reader, JsonElement.class));
    }
  }

  /**
   * Parses a json element from a string
   */
  public static JsonElement jsonFromString(String s) {
    return gson.fromJson(s, JsonElement.class);
  }


  /** Read an ADL value from stdin */
  public static <T> T fromStdin(JsonBinding<T> binding) {
    InputStreamReader utf8Reader = new InputStreamReader(System.in);
    return binding.fromJson(gson.fromJson(utf8Reader, JsonElement.class));
  }

  /** Read an ADL value leniently from stdin */
  public static <T> T fromLenientStdin(JsonBinding<T> binding) {
    InputStreamReader utf8Reader = new InputStreamReader(System.in);
    return binding.fromJson(lenientGson.fromJson(utf8Reader, JsonElement.class));
  }

  /**
   * Merge 2 json values such that they can subsequently be parsed.
   *
   * Json objects get merged with the values in jv2 overiding those in
   * jv1 (including nested objects). For any other types the
   * value in jv2 replaces the value in jv1.
   */
  public static JsonElement mergeJson(JsonElement jv1, JsonElement jv2) {
    if( jv1.isJsonObject() && jv2.isJsonObject()) {
      JsonObject jobj1 = jv1.getAsJsonObject();
      JsonObject jobj2 = jv2.getAsJsonObject();

      JsonObject result = new JsonObject();
      String lastMergedField = null;

      for(Map.Entry<String,JsonElement> me : jobj1.entrySet()) {
        if (!me.getKey().equals(LAST_MERGED_FIELD)) {
          result.add(me.getKey(), me.getValue());
          lastMergedField = me.getKey();
        }
      }
      for(Map.Entry<String,JsonElement> me : jobj2.entrySet()) {
        if (!me.getKey().equals(LAST_MERGED_FIELD)) {
          if (result.has(me.getKey())) {
            result.add(me.getKey(), mergeJson(result.get(me.getKey()), me.getValue()));
          } else {
            result.add(me.getKey(), me.getValue());
          }
          lastMergedField = me.getKey();
        }
      }

      if (Objects.nonNull(lastMergedField)) {
        result.add(LAST_MERGED_FIELD, new JsonPrimitive(lastMergedField));
      }
      return result;
    } else {
      return jv2;
    }
  }
};
