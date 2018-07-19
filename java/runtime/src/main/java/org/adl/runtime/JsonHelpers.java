package org.adl.runtime;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.nio.charset.Charset;

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
};
