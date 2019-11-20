/* @generated from adl module admin */

package adl.admin;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.TypeToken;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class HttpGet<O> {

  /* Members */

  private String path;
  private TypeToken<O> otype;

  /* Constructors */

  public HttpGet(String path, TypeToken<O> otype) {
    this.path = Objects.requireNonNull(path);
    this.otype = Objects.requireNonNull(otype);
  }

  /* Accessors and mutators */

  public String getPath() {
    return path;
  }

  public void setPath(String path) {
    this.path = Objects.requireNonNull(path);
  }

  public TypeToken<O> getOtype() {
    return otype;
  }

  public void setOtype(TypeToken<O> otype) {
    this.otype = Objects.requireNonNull(otype);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof HttpGet)) {
      return false;
    }
    HttpGet<?> other = (HttpGet<?>) other0;
    return
      path.equals(other.path) &&
      otype.equals(other.otype);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + path.hashCode();
    _result = _result * 37 + otype.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static <O> Factory<HttpGet<O>> factory(Factory<O> factoryO) {
    return new Factory<HttpGet<O>>() {
      final Lazy<Factory<String>> path = new Lazy<>(() -> Factories.STRING);
      final Lazy<Factory<TypeToken<O>>> otype = new Lazy<>(() -> Factories.typeProxy(factoryO));

      @Override
      public HttpGet<O> create() {
        return new HttpGet<O>(
          path.get().create(),
          new TypeToken<O>(factoryO.jsonBinding())
          );
      }

      @Override
      public HttpGet<O> create(HttpGet<O> other) {
        return new HttpGet<O>(
          other.getPath(),
          other.getOtype()
          );
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("admin", "HttpGet");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryO.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }

      @Override
      public JsonBinding<HttpGet<O>> jsonBinding() {
        return HttpGet.jsonBinding(factoryO.jsonBinding());
      }
    };
  }

  /* Json serialization */

  public static<O> JsonBinding<HttpGet<O>> jsonBinding(JsonBinding<O> bindingO) {
    final Lazy<JsonBinding<String>> path = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<TypeToken<O>>> otype = new Lazy<>(() -> JsonBindings.typeProxy(bindingO));
    final Factory<O> factoryO = bindingO.factory();
    final Factory<HttpGet<O>> _factory = factory(bindingO.factory());

    return new JsonBinding<HttpGet<O>>() {
      @Override
      public Factory<HttpGet<O>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(HttpGet<O> _value) {
        JsonObject _result = new JsonObject();
        _result.add("path", path.get().toJson(_value.path));
        _result.add("otype", otype.get().toJson(_value.otype));
        return _result;
      }

      @Override
      public HttpGet<O> fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new HttpGet<O>(
          JsonBindings.fieldFromJson(_obj, "path", path.get()),
          _obj.has("otype") ? JsonBindings.fieldFromJson(_obj, "otype", otype.get()) : new TypeToken<O>(bindingO)
        );
      }
    };
  }
}
