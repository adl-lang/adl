/* @generated from adl module test7 */

package adl.test7;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class Point2<X> {

  /* Members */

  private Point<X> value;

  /* Constructors */

  public Point2(Point<X> value) {
    this.value = Objects.requireNonNull(value);
  }

  /* Accessors and mutators */

  public Point<X> getValue() {
    return value;
  }

  public void setValue(Point<X> value) {
    this.value = Objects.requireNonNull(value);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Point2)) {
      return false;
    }
    Point2<?> other = (Point2<?>) other0;
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

  public static <X> Factory<Point2<X>> factory(Factory<X> factoryX) {
    return new Factory<Point2<X>>() {
      final Lazy<Factory<Point<X>>> value = new Lazy<>(() -> Point.factory(factoryX));

      @Override
      public Point2<X> create() {
        return new Point2<X>(
          value.get().create()
          );
      }

      @Override
      public Point2<X> create(Point2<X> other) {
        return new Point2<X>(
          value.get().create(other.getValue())
          );
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("test7", "Point2");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryX.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }

      @Override
      public JsonBinding<Point2<X>> jsonBinding() {
        return Point2.jsonBinding(factoryX.jsonBinding());
      }
    };
  }

  /* Json serialization */

  public static<X> JsonBinding<Point2<X>> jsonBinding(JsonBinding<X> bindingX) {
    final JsonBinding<Point<X>> _binding = Point.jsonBinding(bindingX);
    final Factory<Point2<X>> _factory = factory(bindingX.factory());

    return new JsonBinding<Point2<X>>() {
      @Override
      public Factory<Point2<X>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Point2<X> _value) {
        return _binding.toJson(_value.value);
      }

      @Override
      public Point2<X> fromJson(JsonElement _json) {
        return new Point2<X>(_binding.fromJson(_json));
      }
    };
  }
}
