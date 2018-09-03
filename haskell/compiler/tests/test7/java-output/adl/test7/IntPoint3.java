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

public class IntPoint3 {

  /* Members */

  private Point<Long> value;

  /* Constructors */

  public IntPoint3(Point<Long> value) {
    this.value = Objects.requireNonNull(value);
  }

  public IntPoint3() {
    this.value = new Point<Long>(5L, 27L);
  }

  public IntPoint3(IntPoint3 other) {
    this.value = Point.factory(Factories.INT64).create(other.value);
  }

  /* Accessors and mutators */

  public Point<Long> getValue() {
    return value;
  }

  public void setValue(Point<Long> value) {
    this.value = Objects.requireNonNull(value);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof IntPoint3)) {
      return false;
    }
    IntPoint3 other = (IntPoint3) other0;
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

  public static final Factory<IntPoint3> FACTORY = new Factory<IntPoint3>() {
    @Override
    public IntPoint3 create() {
      return new IntPoint3();
    }

    @Override
    public IntPoint3 create(IntPoint3 other) {
      return new IntPoint3(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test7", "IntPoint3");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
  };

  /* Json serialization */

  public static JsonBinding<IntPoint3> jsonBinding() {
    final JsonBinding<Point<Long>> _binding = Point.jsonBinding(JsonBindings.INT64);
    final Factory<IntPoint3> _factory = FACTORY;

    return new JsonBinding<IntPoint3>() {
      @Override
      public Factory<IntPoint3> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(IntPoint3 _value) {
        return _binding.toJson(_value.value);
      }

      @Override
      public IntPoint3 fromJson(JsonElement _json) {
        return new IntPoint3(_binding.fromJson(_json));
      }
    };
  }
}
