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

public class Int5<X> {

  /* Members */

  private long value;

  /* Constructors */

  public Int5(long value) {
    this.value = value;
  }

  /* Accessors and mutators */

  public long getValue() {
    return value;
  }

  public void setValue(long value) {
    this.value = value;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Int5)) {
      return false;
    }
    Int5<?> other = (Int5<?>) other0;
    return
      value == other.value;
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + (int) (value ^ (value >>> 32));
    return _result;
  }

  /* Factory for construction of generic values */

  public static <X> Factory<Int5<X>> factory(Factory<X> factoryX) {
    return new Factory<Int5<X>>() {
      final Lazy<Factory<Long>> value = new Lazy<>(() -> Factories.INT64);

      @Override
      public Int5<X> create(Int5<X> other) {
        return new Int5<X>(
          other.getValue()
          );
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("test7", "Int5");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryX.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }

      @Override
      public JsonBinding<Int5<X>> jsonBinding() {
        return Int5.jsonBinding(factoryX.jsonBinding());
      }
    };
  }

  /* Json serialization */

  public static<X> JsonBinding<Int5<X>> jsonBinding(JsonBinding<X> bindingX) {
    final JsonBinding<Long> _binding = JsonBindings.INT64;
    final Factory<Int5<X>> _factory = factory(bindingX.factory());

    return new JsonBinding<Int5<X>>() {
      @Override
      public Factory<Int5<X>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Int5<X> _value) {
        return _binding.toJson(_value.value);
      }

      @Override
      public Int5<X> fromJson(JsonElement _json) {
        return new Int5<X>(_binding.fromJson(_json));
      }
    };
  }
}
