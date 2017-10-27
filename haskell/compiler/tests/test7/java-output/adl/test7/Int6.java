/* Code generated from adl module test7 */

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

public class Int6<X> {

  /* Members */

  private long value;

  /* Constructors */

  public Int6(long value) {
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
    if (!(other0 instanceof Int6)) {
      return false;
    }
    Int6<?> other = (Int6<?>) other0;
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

  public static <X> Factory<Int6<X>> factory(Factory<X> factoryX) {
    return new Factory<Int6<X>>() {
      final Lazy<Factory<Long>> value = new Lazy<>(() -> Factories.INT64);

      @Override
      public Int6<X> create() {
        return new Int6<X>(
          43L
          );
      }

      @Override
      public Int6<X> create(Int6<X> other) {
        return new Int6<X>(
          other.getValue()
          );
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("test7", "Int6");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryX.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }
    };
  }

  /* Json serialization */

  public static<X> JsonBinding<Int6<X>> jsonBinding(JsonBinding<X> bindingX) {
    final JsonBinding<Long> _binding = JsonBindings.INT64;
    final Factory<Int6<X>> _factory = factory(bindingX.factory());

    return new JsonBinding<Int6<X>>() {
      @Override
      public Factory<Int6<X>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Int6<X> _value) {
        return _binding.toJson(_value.value);
      }

      @Override
      public Int6<X> fromJson(JsonElement _json) {
        return new Int6<X>(_binding.fromJson(_json));
      }
    };
  }
}
