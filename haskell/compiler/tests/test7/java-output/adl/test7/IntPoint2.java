/* @generated from adl module test7 */

package adl.test7;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class IntPoint2 {

  /* Members */

  private Point<Long> value;

  /* Constructors */

  public IntPoint2(Point<Long> value) {
    this.value = Objects.requireNonNull(value);
  }

  public IntPoint2(IntPoint2 other) {
    this.value = Point.factory(Factories.INT64).create(other.value);
  }

  /* Accessors and mutators */

  public Point<Long> getValue() {
    return value;
  }

  public IntPoint2 setValue(Point<Long> value) {
    this.value = Objects.requireNonNull(value);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof IntPoint2)) {
      return false;
    }
    IntPoint2 other = (IntPoint2) other0;
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

  public static final Factory<IntPoint2> FACTORY = new Factory<IntPoint2>() {
    @Override
    public IntPoint2 create(IntPoint2 other) {
      return new IntPoint2(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test7", "IntPoint2");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<IntPoint2> jsonBinding() {
      return IntPoint2.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<IntPoint2> jsonBinding() {
    final JsonBinding<Point<Long>> _binding = Point.jsonBinding(JsonBindings.INT64);
    final Factory<IntPoint2> _factory = FACTORY;

    return new JsonBinding<IntPoint2>() {
      @Override
      public Factory<IntPoint2> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(IntPoint2 _value) {
        return _binding.toJson(_value.value);
      }

      @Override
      public IntPoint2 fromJson(JsonElement _json) {
        return new IntPoint2(_binding.fromJson(_json));
      }
    };
  }
}
