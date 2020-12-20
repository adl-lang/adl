/* @generated from adl module test2 */

package adl.test2;

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
import java.util.List;
import java.util.Objects;

/**
 * A structure containing a vector.
 */
public class S2 {

  /* Members */

  private String f1;
  private double f2;
  private List<Integer> f3;

  /* Constructors */

  public S2(String f1, double f2, List<Integer> f3) {
    this.f1 = Objects.requireNonNull(f1);
    this.f2 = f2;
    this.f3 = Objects.requireNonNull(f3);
  }

  public S2(S2 other) {
    this.f1 = other.f1;
    this.f2 = other.f2;
    this.f3 = Factories.list(Factories.INT32).create(other.f3);
  }

  /* Accessors and mutators */

  public String getF1() {
    return f1;
  }

  public void setF1(String f1) {
    this.f1 = Objects.requireNonNull(f1);
  }

  public double getF2() {
    return f2;
  }

  public void setF2(double f2) {
    this.f2 = f2;
  }

  public List<Integer> getF3() {
    return f3;
  }

  public void setF3(List<Integer> f3) {
    this.f3 = Objects.requireNonNull(f3);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S2)) {
      return false;
    }
    S2 other = (S2) other0;
    return
      f1.equals(other.f1) &&
      f2 == other.f2 &&
      f3.equals(other.f3);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + f1.hashCode();
    _result = _result * 37 + Double.valueOf(f2).hashCode();
    _result = _result * 37 + f3.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder {
    private String f1;
    private Double f2;
    private List<Integer> f3;

    public Builder() {
      this.f1 = null;
      this.f2 = null;
      this.f3 = null;
    }

    public Builder setF1(String f1) {
      this.f1 = Objects.requireNonNull(f1);
      return this;
    }

    public Builder setF2(Double f2) {
      this.f2 = Objects.requireNonNull(f2);
      return this;
    }

    public Builder setF3(List<Integer> f3) {
      this.f3 = Objects.requireNonNull(f3);
      return this;
    }

    public S2 create() {
      Builders.checkFieldInitialized("S2", "f1", f1);
      Builders.checkFieldInitialized("S2", "f2", f2);
      Builders.checkFieldInitialized("S2", "f3", f3);
      return new S2(f1, f2, f3);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<S2> FACTORY = new Factory<S2>() {
    @Override
    public S2 create(S2 other) {
      return new S2(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test2", "S2");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<S2> jsonBinding() {
      return S2.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<S2> jsonBinding() {
    final Lazy<JsonBinding<String>> f1 = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<Double>> f2 = new Lazy<>(() -> JsonBindings.DOUBLE);
    final Lazy<JsonBinding<List<Integer>>> f3 = new Lazy<>(() -> JsonBindings.list(JsonBindings.INT32));
    final Factory<S2> _factory = FACTORY;

    return new JsonBinding<S2>() {
      @Override
      public Factory<S2> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(S2 _value) {
        JsonObject _result = new JsonObject();
        _result.add("f1", f1.get().toJson(_value.f1));
        _result.add("f2", f2.get().toJson(_value.f2));
        _result.add("f3", f3.get().toJson(_value.f3));
        return _result;
      }

      @Override
      public S2 fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new S2(
          JsonBindings.fieldFromJson(_obj, "f1", f1.get()),
          JsonBindings.fieldFromJson(_obj, "f2", f2.get()),
          JsonBindings.fieldFromJson(_obj, "f3", f3.get())
        );
      }
    };
  }
}
