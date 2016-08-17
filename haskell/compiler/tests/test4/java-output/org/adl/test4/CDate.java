package org.adl.test4;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import java.util.Objects;

public class CDate {

  /* Members */

  private short year;
  private short month;
  private short day;

  /* Constructors */

  public CDate(short year, short month, short day) {
    this.year = year;
    this.month = month;
    this.day = day;
  }

  public CDate() {
    this.year = (short)0;
    this.month = (short)0;
    this.day = (short)0;
  }

  public CDate(CDate other) {
    this.year = other.year;
    this.month = other.month;
    this.day = other.day;
  }

  /* Accessors and mutators */

  public short getYear() {
    return year;
  }

  public void setYear(short year) {
    this.year = year;
  }

  public short getMonth() {
    return month;
  }

  public void setMonth(short month) {
    this.month = month;
  }

  public short getDay() {
    return day;
  }

  public void setDay(short day) {
    this.day = day;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof CDate)) {
      return false;
    }
    CDate other = (CDate) other0;
    return
      year == other.year &&
      month == other.month &&
      day == other.day;
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + (int) year;
    result = result * 37 + (int) month;
    result = result * 37 + (int) day;
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<CDate> FACTORY = new Factory<CDate>() {
    public CDate create() {
      return new CDate();
    }
    public CDate create(CDate other) {
      return new CDate(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<CDate> jsonBinding() {
    final JsonBinding<Short> year = JsonBindings.SHORT;
    final JsonBinding<Short> month = JsonBindings.SHORT;
    final JsonBinding<Short> day = JsonBindings.SHORT;
    final Factory<CDate> _factory = FACTORY;

    return new JsonBinding<CDate>() {
      public Factory<CDate> factory() {
        return _factory;
      }

      public JsonElement toJson(CDate _value) {
        JsonObject _result = new JsonObject();
        _result.add("year", year.toJson(_value.year));
        _result.add("month", month.toJson(_value.month));
        _result.add("day", day.toJson(_value.day));
        return _result;
      }

      public CDate fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new CDate(
          _obj.has("year") ? year.fromJson(_obj.get("year")) : (short)0,
          _obj.has("month") ? month.fromJson(_obj.get("month")) : (short)0,
          _obj.has("day") ? day.fromJson(_obj.get("day")) : (short)0
        );
      }
    };
  }
}
