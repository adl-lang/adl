/* Code generated from adl module test4 */

package org.adl.test4;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import java.util.Objects;

public class Date {

  /* Members */

  private String value;

  /* Constructors */

  public Date(String value) {
    this.value = Objects.requireNonNull(value);
  }

  public Date() {
    this.value = "1900-01-01";
  }

  public Date(Date other) {
    this.value = other.value;
  }

  /* Accessors and mutators */

  public String getValue() {
    return value;
  }

  public void setValue(String value) {
    this.value = Objects.requireNonNull(value);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Date)) {
      return false;
    }
    Date other = (Date) other0;
    return
      value.equals(other.value);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + value.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<Date> FACTORY = new Factory<Date>() {
    public Date create() {
      return new Date();
    }
    public Date create(Date other) {
      return new Date(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<Date> jsonBinding() {
    final JsonBinding<String> _binding = JsonBindings.STRING;
    final Factory<Date> _factory = FACTORY;

    return new JsonBinding<Date>() {
      public Factory<Date> factory() {
        return _factory;
      }

      public JsonElement toJson(Date _value) {
        return _binding.toJson(_value.value);
      }

      public Date fromJson(JsonElement _json) {
        return new Date(_binding.fromJson(_json));
      }
    };
  }
}
