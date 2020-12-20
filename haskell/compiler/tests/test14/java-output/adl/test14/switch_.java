/* @generated from adl module test14 */

package adl.test14;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Builders;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;

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

  /* Builder */

  public static class Builder {
    private Double double_;
    private Integer int_;
    private String string;
    private Boolean for_;
    private String Objects;

    public Builder() {
      this.double_ = null;
      this.int_ = null;
      this.string = null;
      this.for_ = null;
      this.Objects = null;
    }

    public Builder setDouble(Double double_) {
      this.double_ = java.util.Objects.requireNonNull(double_);
      return this;
    }

    public Builder setInt(Integer int_) {
      this.int_ = java.util.Objects.requireNonNull(int_);
      return this;
    }

    public Builder setString(String string) {
      this.string = java.util.Objects.requireNonNull(string);
      return this;
    }

    public Builder setFor(Boolean for_) {
      this.for_ = java.util.Objects.requireNonNull(for_);
      return this;
    }

    public Builder setObjects(String Objects) {
      this.Objects = java.util.Objects.requireNonNull(Objects);
      return this;
    }

    public switch_ create() {
      Builders.checkFieldInitialized("switch_", "double_", double_);
      Builders.checkFieldInitialized("switch_", "int_", int_);
      Builders.checkFieldInitialized("switch_", "string", string);
      Builders.checkFieldInitialized("switch_", "for_", for_);
      Builders.checkFieldInitialized("switch_", "Objects", Objects);
      return new switch_(double_, int_, string, for_, Objects);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<switch_> FACTORY = new Factory<switch_>() {
    @Override
    public switch_ create(switch_ other) {
      return new switch_(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test14", "switch_");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<switch_> jsonBinding() {
      return switch_.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<switch_> jsonBinding() {
    final Lazy<JsonBinding<Double>> double_ = new Lazy<>(() -> JsonBindings.DOUBLE);
    final Lazy<JsonBinding<Integer>> int_ = new Lazy<>(() -> JsonBindings.INT32);
    final Lazy<JsonBinding<String>> string = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<Boolean>> for_ = new Lazy<>(() -> JsonBindings.BOOLEAN);
    final Lazy<JsonBinding<String>> Objects = new Lazy<>(() -> JsonBindings.STRING);
    final Factory<switch_> _factory = FACTORY;

    return new JsonBinding<switch_>() {
      @Override
      public Factory<switch_> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(switch_ _value) {
        JsonObject _result = new JsonObject();
        _result.add("double", double_.get().toJson(_value.double_));
        _result.add("int", int_.get().toJson(_value.int_));
        _result.add("string", string.get().toJson(_value.string));
        _result.add("for", for_.get().toJson(_value.for_));
        _result.add("Objects", Objects.get().toJson(_value.Objects));
        return _result;
      }

      @Override
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
