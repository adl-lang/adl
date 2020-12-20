/* @generated from adl module test5 */

package adl.test5;

import com.google.gson.JsonElement;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.JsonParseException;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class List<T> {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The List discriminator type.
   */
  public enum Disc {
    NULL_,
    CELL
  }

  /* Constructors */

  public static <T> List<T> null_() {
    return new List<T>(Disc.NULL_, null);
  }

  public static <T> List<T> cell(Cell<T> v) {
    return new List<T>(Disc.CELL, Objects.requireNonNull(v));
  }

  private List(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public Cell<T> getCell() {
    if (disc == Disc.CELL) {
      return List.<Cell<T>>cast(value);
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setNull() {
    this.value = null;
    this.disc = Disc.NULL_;
  }

  public void setCell(Cell<T> v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.CELL;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof List)) {
      return false;
    }
    List<?> other = (List<?>) other0;
    switch (disc) {
      case NULL_:
        return disc == other.disc;
      case CELL:
        return disc == other.disc && value.equals(other.value);
    }
    throw new IllegalStateException();
  }

  @Override
  public int hashCode() {
    switch (disc) {
      case NULL_:
        return disc.hashCode();
      case CELL:
        return disc.hashCode() * 37 + value.hashCode();
    }
    throw new IllegalStateException();
  }

  @SuppressWarnings("unchecked")
  private static <T> T cast(final Object o) {
    return (T) o;
  }

  /* Factory for construction of generic values */

  public static <T> Factory<List <T>> factory(Factory<T> factoryT) {
    return new Factory<List<T>>() {
      final Lazy<Factory<Void>> null_ = new Lazy<>(() -> Factories.VOID);
      final Lazy<Factory<Cell<T>>> cell = new Lazy<>(() -> Cell.factory(factoryT));


      @Override
      public List<T> create(List<T> other) {
        switch (other.disc) {
          case NULL_:
            return new List<T>(other.disc,other.value);
          case CELL:
            return new List<T>(other.disc,cell.get().create(List.<Cell<T>>cast(other.value)));
        }
        throw new IllegalArgumentException();
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("test5", "List");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryT.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }

      @Override
      public JsonBinding<List<T>> jsonBinding() {
        return List.jsonBinding(factoryT.jsonBinding());
      }
    };
  }

  /* Json serialization */

  public static<T> JsonBinding<List<T>> jsonBinding(JsonBinding<T> bindingT) {
    final Lazy<JsonBinding<Void>> null_ = new Lazy<>(() -> JsonBindings.VOID);
    final Lazy<JsonBinding<Cell<T>>> cell = new Lazy<>(() -> Cell.jsonBinding(bindingT));
    final Factory<T> factoryT = bindingT.factory();
    final Factory<List<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<List<T>>() {
      @Override
      public Factory<List<T>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(List<T> _value) {
        switch (_value.getDisc()) {
          case NULL_:
            return JsonBindings.unionToJson("null", null, null);
          case CELL:
            return JsonBindings.unionToJson("cell", _value.getCell(), cell.get());
        }
        return null;
      }

      @Override
      public List<T> fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("null")) {
          return List.<T>null_();
        }
        else if (_key.equals("cell")) {
          return List.<T>cell(JsonBindings.unionValueFromJson(_json, cell.get()));
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union List<T>");
      }
    };
  }
}
