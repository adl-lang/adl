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
import java.util.Objects;

/**
 * A structure containing primitives.
 * 
 * It has two fields: an integer x and a String y.
 */
public class S1 {

  /* Members */

  private int x;
  private String y;

  /* Constructors */

  public S1(int x, String y) {
    this.x = x;
    this.y = Objects.requireNonNull(y);
  }

  public S1() {
    this.x = 0;
    this.y = "";
  }

  public S1(S1 other) {
    this.x = other.x;
    this.y = other.y;
  }

  /* Accessors and mutators */

  public int getX() {
    return x;
  }

  public void setX(int x) {
    this.x = x;
  }

  public String getY() {
    return y;
  }

  public void setY(String y) {
    this.y = Objects.requireNonNull(y);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S1)) {
      return false;
    }
    S1 other = (S1) other0;
    return
      x == other.x &&
      y.equals(other.y);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + x;
    _result = _result * 37 + y.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder {
    private Integer x;
    private String y;

    public Builder() {
      this.x = null;
      this.y = null;
    }

    public Builder setX(Integer x) {
      this.x = Objects.requireNonNull(x);
      return this;
    }

    public Builder setY(String y) {
      this.y = Objects.requireNonNull(y);
      return this;
    }

    public S1 create() {
      Builders.checkFieldInitialized("S1", "x", x);
      Builders.checkFieldInitialized("S1", "y", y);
      return new S1(x, y);
    }
  }

  /* Factory for construction of generic values */

  public static final Factory<S1> FACTORY = new Factory<S1>() {
    @Override
    public S1 create() {
      return new S1();
    }

    @Override
    public S1 create(S1 other) {
      return new S1(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test2", "S1");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
  };

  /* Json serialization */

  public static JsonBinding<S1> jsonBinding() {
    final Lazy<JsonBinding<Integer>> x = new Lazy<>(() -> JsonBindings.INT32);
    final Lazy<JsonBinding<String>> y = new Lazy<>(() -> JsonBindings.STRING);
    final Factory<S1> _factory = FACTORY;

    return new JsonBinding<S1>() {
      @Override
      public Factory<S1> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(S1 _value) {
        JsonObject _result = new JsonObject();
        _result.add("x", x.get().toJson(_value.x));
        _result.add("y", y.get().toJson(_value.y));
        return _result;
      }

      @Override
      public S1 fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new S1(
          JsonBindings.fieldFromJson(_obj, "x", x.get()),
          JsonBindings.fieldFromJson(_obj, "y", y.get())
        );
      }
    };
  }
}
