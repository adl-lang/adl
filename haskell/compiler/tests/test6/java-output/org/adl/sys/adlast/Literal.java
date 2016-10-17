package org.adl.sys.adlast;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.HashMapHelpers;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class Literal {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The Literal discriminator type.
   */
  public enum Disc {
    NULL_,
    INTEGER,
    DOUBLE_,
    STRING,
    BOOLEAN_,
    ARRAY,
    OBJECT
  }

  /* Constructors */

  public static Literal null_() {
    return new Literal(Disc.NULL_, null);
  }

  public static Literal integer(long v) {
    return new Literal(Disc.INTEGER, v);
  }

  public static Literal double_(double v) {
    return new Literal(Disc.DOUBLE_, v);
  }

  public static Literal string(String v) {
    return new Literal(Disc.STRING, Objects.requireNonNull(v));
  }

  public static Literal boolean_(boolean v) {
    return new Literal(Disc.BOOLEAN_, v);
  }

  public static Literal array(ArrayList<Literal> v) {
    return new Literal(Disc.ARRAY, Objects.requireNonNull(v));
  }

  public static Literal object(HashMap<String, Literal> v) {
    return new Literal(Disc.OBJECT, Objects.requireNonNull(v));
  }

  public Literal() {
    this.disc = Disc.NULL_;
    this.value = null;
  }

  public Literal(Literal other) {
    this.disc = other.disc;
    switch (other.disc) {
      case NULL_:
        this.value = (Void) other.value;
        break;
      case INTEGER:
        this.value = (Long) other.value;
        break;
      case DOUBLE_:
        this.value = (Double) other.value;
        break;
      case STRING:
        this.value = (String) other.value;
        break;
      case BOOLEAN_:
        this.value = (Boolean) other.value;
        break;
      case ARRAY:
        this.value = Factories.arrayList(Literal.FACTORY).create(Literal.<ArrayList<Literal>>cast(other.value));
        break;
      case OBJECT:
        this.value = HashMapHelpers.factory(Factories.STRING, Literal.FACTORY).create(Literal.<HashMap<String, Literal>>cast(other.value));
        break;
    }
  }

  private Literal(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public long getInteger() {
    if (disc == Disc.INTEGER) {
      return (Long) value;
    }
    throw new IllegalStateException();
  }

  public double getDouble() {
    if (disc == Disc.DOUBLE_) {
      return (Double) value;
    }
    throw new IllegalStateException();
  }

  public String getString() {
    if (disc == Disc.STRING) {
      return (String) value;
    }
    throw new IllegalStateException();
  }

  public boolean getBoolean() {
    if (disc == Disc.BOOLEAN_) {
      return (Boolean) value;
    }
    throw new IllegalStateException();
  }

  public ArrayList<Literal> getArray() {
    if (disc == Disc.ARRAY) {
      return Literal.<ArrayList<Literal>>cast(value);
    }
    throw new IllegalStateException();
  }

  public HashMap<String, Literal> getObject() {
    if (disc == Disc.OBJECT) {
      return Literal.<HashMap<String, Literal>>cast(value);
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setNull() {
    this.value = null;
    this.disc = Disc.NULL_;
  }

  public void setInteger(long v) {
    this.value = v;
    this.disc = Disc.INTEGER;
  }

  public void setDouble(double v) {
    this.value = v;
    this.disc = Disc.DOUBLE_;
  }

  public void setString(String v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.STRING;
  }

  public void setBoolean(boolean v) {
    this.value = v;
    this.disc = Disc.BOOLEAN_;
  }

  public void setArray(ArrayList<Literal> v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.ARRAY;
  }

  public void setObject(HashMap<String, Literal> v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.OBJECT;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Literal)) {
      return false;
    }
    Literal other = (Literal) other0;
    switch (disc) {
      case NULL_:
        return disc == other.disc;
      case INTEGER:
        return disc == other.disc && value.equals(other.value);
      case DOUBLE_:
        return disc == other.disc && value.equals(other.value);
      case STRING:
        return disc == other.disc && value.equals(other.value);
      case BOOLEAN_:
        return disc == other.disc && value.equals(other.value);
      case ARRAY:
        return disc == other.disc && value.equals(other.value);
      case OBJECT:
        return disc == other.disc && value.equals(other.value);
    }
    throw new IllegalStateException();
  }

  @Override
  public int hashCode() {
    switch (disc) {
      case NULL_:
        return disc.hashCode();
      case INTEGER:
        return disc.hashCode() * 37 + value.hashCode();
      case DOUBLE_:
        return disc.hashCode() * 37 + value.hashCode();
      case STRING:
        return disc.hashCode() * 37 + value.hashCode();
      case BOOLEAN_:
        return disc.hashCode() * 37 + value.hashCode();
      case ARRAY:
        return disc.hashCode() * 37 + value.hashCode();
      case OBJECT:
        return disc.hashCode() * 37 + value.hashCode();
    }
    throw new IllegalStateException();
  }

  @SuppressWarnings("unchecked")
  private static <T> T cast(final Object o) {
    return (T) o;
  }

  /* Factory for construction of generic values */

  public static final Factory<Literal> FACTORY = new Factory<Literal>() {
    public Literal create() {
      return new Literal();
    }
    public Literal create(Literal other) {
      return new Literal(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<Literal> jsonBinding() {
    final Lazy<JsonBinding<Void>> null_ = new Lazy<>(() -> JsonBindings.VOID);
    final Lazy<JsonBinding<Long>> integer = new Lazy<>(() -> JsonBindings.LONG);
    final Lazy<JsonBinding<Double>> double_ = new Lazy<>(() -> JsonBindings.DOUBLE);
    final Lazy<JsonBinding<String>> string = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<Boolean>> boolean_ = new Lazy<>(() -> JsonBindings.BOOLEAN);
    final Lazy<JsonBinding<ArrayList<Literal>>> array = new Lazy<>(() -> JsonBindings.arrayList(org.adl.sys.adlast.Literal.jsonBinding()));
    final Lazy<JsonBinding<HashMap<String, Literal>>> object = new Lazy<>(() -> HashMapHelpers.jsonBinding(JsonBindings.STRING, org.adl.sys.adlast.Literal.jsonBinding()));
    final Factory<Literal> _factory = FACTORY;

    return new JsonBinding<Literal>() {
      public Factory<Literal> factory() {
        return _factory;
      }

      public JsonElement toJson(Literal _value) {
        JsonObject _result = new JsonObject();
        switch (_value.getDisc()) {
          case NULL_:
            _result.add("null", null);
          case INTEGER:
            _result.add("integer", integer.get().toJson(_value.getInteger()));
            break;
          case DOUBLE_:
            _result.add("double", double_.get().toJson(_value.getDouble()));
            break;
          case STRING:
            _result.add("string", string.get().toJson(_value.getString()));
            break;
          case BOOLEAN_:
            _result.add("boolean", boolean_.get().toJson(_value.getBoolean()));
            break;
          case ARRAY:
            _result.add("array", array.get().toJson(_value.getArray()));
            break;
          case OBJECT:
            _result.add("object", object.get().toJson(_value.getObject()));
            break;
        }
        return _result;
      }

      public Literal fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        for (Map.Entry<String,JsonElement> _v : _obj.entrySet()) {
          if (_v.getKey().equals("null")) {
            return Literal.null_();
          }
          else if (_v.getKey().equals("integer")) {
            return Literal.integer(integer.get().fromJson(_v.getValue()));
          }
          else if (_v.getKey().equals("double")) {
            return Literal.double_(double_.get().fromJson(_v.getValue()));
          }
          else if (_v.getKey().equals("string")) {
            return Literal.string(string.get().fromJson(_v.getValue()));
          }
          else if (_v.getKey().equals("boolean")) {
            return Literal.boolean_(boolean_.get().fromJson(_v.getValue()));
          }
          else if (_v.getKey().equals("array")) {
            return Literal.array(array.get().fromJson(_v.getValue()));
          }
          else if (_v.getKey().equals("object")) {
            return Literal.object(object.get().fromJson(_v.getValue()));
          }
        }
        throw new IllegalStateException();
      }
    };
  }
}
