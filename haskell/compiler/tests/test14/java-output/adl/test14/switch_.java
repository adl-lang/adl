/* Code generated from adl module test14 */

package adl.test14;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;

public class switch_ {

  /* Members */

  private double double_;
  private int int_;
  private String string;
  private boolean for_;
  private String Objects;

  /* Constructors */

  public switch_(double double_, int int_, String string, boolean for_, String Objects) {
    this.double_ = double_;
    this.int_ = int_;
    this.string = java.util.Objects.requireNonNull(string);
    this.for_ = for_;
    this.Objects = java.util.Objects.requireNonNull(Objects);
  }

  public switch_() {
    this.double_ = 0.0;
    this.int_ = 0;
    this.string = "";
    this.for_ = false;
    this.Objects = "";
  }

  public switch_(switch_ other) {
    this.double_ = other.double_;
    this.int_ = other.int_;
    this.string = other.string;
    this.for_ = other.for_;
    this.Objects = other.Objects;
  }

  /* Accessors and mutators */

  public double getDouble() {
    return double_;
  }

  public void setDouble(double double_) {
    this.double_ = double_;
  }

  public int getInt() {
    return int_;
  }

  public void setInt(int int_) {
    this.int_ = int_;
  }

  public String getString() {
    return string;
  }

  public void setString(String string) {
    this.string = java.util.Objects.requireNonNull(string);
  }

  public boolean getFor() {
    return for_;
  }

  public void setFor(boolean for_) {
    this.for_ = for_;
  }

  public String getObjects() {
    return Objects;
  }

  public void setObjects(String Objects) {
    this.Objects = java.util.Objects.requireNonNull(Objects);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof switch_)) {
      return false;
    }
    switch_ other = (switch_) other0;
    return
      double_ == other.double_ &&
      int_ == other.int_ &&
      string.equals(other.string) &&
      for_ == other.for_ &&
      Objects.equals(other.Objects);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + Double.valueOf(double_).hashCode();
    _result = _result * 37 + int_;
    _result = _result * 37 + string.hashCode();
    _result = _result * 37 + (for_ ? 0 : 1);
    _result = _result * 37 + Objects.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<switch_> FACTORY = new Factory<switch_>() {
    public switch_ create() {
      return new switch_();
    }
    public switch_ create(switch_ other) {
      return new switch_(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<switch_> jsonBinding() {
    final Lazy<JsonBinding<Double>> double_ = new Lazy<>(() -> JsonBindings.DOUBLE);
    final Lazy<JsonBinding<Integer>> int_ = new Lazy<>(() -> JsonBindings.INTEGER);
    final Lazy<JsonBinding<String>> string = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<Boolean>> for_ = new Lazy<>(() -> JsonBindings.BOOLEAN);
    final Lazy<JsonBinding<String>> Objects = new Lazy<>(() -> JsonBindings.STRING);
    final Factory<switch_> _factory = FACTORY;

    return new JsonBinding<switch_>() {
      public Factory<switch_> factory() {
        return _factory;
      }

      public JsonElement toJson(switch_ _value) {
        JsonObject _result = new JsonObject();
        _result.add("double", double_.get().toJson(_value.double_));
        _result.add("int", int_.get().toJson(_value.int_));
        _result.add("string", string.get().toJson(_value.string));
        _result.add("for", for_.get().toJson(_value.for_));
        _result.add("Objects", Objects.get().toJson(_value.Objects));
        return _result;
      }

      public switch_ fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new switch_(
          JsonBindings.fieldFromJson(_obj, "double", double_.get()),
          JsonBindings.fieldFromJson(_obj, "int", int_.get()),
          JsonBindings.fieldFromJson(_obj, "string", string.get()),
          JsonBindings.fieldFromJson(_obj, "for", for_.get()),
          JsonBindings.fieldFromJson(_obj, "Objects", Objects.get())
        );
      }
    };
  }
}
