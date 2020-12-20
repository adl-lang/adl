/* @generated from adl module test7 */

package adl.test7;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
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

public class S {

  /* Members */

  private Point<Long> f1;

  /* Constructors */

  public S(Point<Long> f1) {
    this.f1 = Objects.requireNonNull(f1);
  }

  public S(S other) {
    this.f1 = Point.factory(Factories.INT64).create(other.f1);
  }

  /* Accessors and mutators */

  public Point<Long> getF1() {
    return f1;
  }

  public void setF1(Point<Long> f1) {
    this.f1 = Objects.requireNonNull(f1);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S)) {
      return false;
    }
    S other = (S) other0;
    return
      f1.equals(other.f1);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + f1.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<S> FACTORY = new Factory<S>() {
    @Override
    public S create(S other) {
      return new S(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test7", "S");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<S> jsonBinding() {
      return S.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<S> jsonBinding() {
    final Lazy<JsonBinding<Point<Long>>> f1 = new Lazy<>(() -> Point.jsonBinding(JsonBindings.INT64));
    final Factory<S> _factory = FACTORY;

    return new JsonBinding<S>() {
      @Override
      public Factory<S> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(S _value) {
        JsonObject _result = new JsonObject();
        _result.add("f1", f1.get().toJson(_value.f1));
        return _result;
      }

      @Override
      public S fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new S(
          JsonBindings.fieldFromJson(_obj, "f1", f1.get())
        );
      }
    };
  }
}
