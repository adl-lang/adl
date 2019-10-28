/* @generated from adl module test24 */

package adl.test24;

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

public class CrudReqs<T> {

  /* Members */

  private PostReq<T, String> create;
  private PostReq<String, T> read;

  /* Constructors */

  public CrudReqs(PostReq<T, String> create, PostReq<String, T> read) {
    this.create = Objects.requireNonNull(create);
    this.read = Objects.requireNonNull(read);
  }

  /* Accessors and mutators */

  public PostReq<T, String> getCreate() {
    return create;
  }

  public void setCreate(PostReq<T, String> create) {
    this.create = Objects.requireNonNull(create);
  }

  public PostReq<String, T> getRead() {
    return read;
  }

  public void setRead(PostReq<String, T> read) {
    this.read = Objects.requireNonNull(read);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof CrudReqs)) {
      return false;
    }
    CrudReqs<?> other = (CrudReqs<?>) other0;
    return
      create.equals(other.create) &&
      read.equals(other.read);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + create.hashCode();
    _result = _result * 37 + read.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder<T> {
    private PostReq<T, String> create;
    private PostReq<String, T> read;

    public Builder() {
      this.create = null;
      this.read = null;
    }

    public Builder<T> setCreate(PostReq<T, String> create) {
      this.create = Objects.requireNonNull(create);
      return this;
    }

    public Builder<T> setRead(PostReq<String, T> read) {
      this.read = Objects.requireNonNull(read);
      return this;
    }

    public CrudReqs<T> create() {
      Builders.checkFieldInitialized("CrudReqs", "create", create);
      Builders.checkFieldInitialized("CrudReqs", "read", read);
      return new CrudReqs<T>(create, read);
    }
  }

  /* Factory for construction of generic values */

  public static <T> Factory<CrudReqs<T>> factory(Factory<T> factoryT) {
    return new Factory<CrudReqs<T>>() {
      final Lazy<Factory<PostReq<T, String>>> create = new Lazy<>(() -> PostReq.factory(factoryT, Factories.STRING));
      final Lazy<Factory<PostReq<String, T>>> read = new Lazy<>(() -> PostReq.factory(Factories.STRING, factoryT));

      @Override
      public CrudReqs<T> create() {
        return new CrudReqs<T>(
          create.get().create(),
          read.get().create()
          );
      }

      @Override
      public CrudReqs<T> create(CrudReqs<T> other) {
        return new CrudReqs<T>(
          create.get().create(other.getCreate()),
          read.get().create(other.getRead())
          );
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("test24", "CrudReqs");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryT.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }

      @Override
      public JsonBinding<CrudReqs<T>> jsonBinding() {
        return CrudReqs.jsonBinding(factoryT.jsonBinding());
      }
    };
  }

  /* Json serialization */

  public static<T> JsonBinding<CrudReqs<T>> jsonBinding(JsonBinding<T> bindingT) {
    final Lazy<JsonBinding<PostReq<T, String>>> create = new Lazy<>(() -> PostReq.jsonBinding(bindingT, JsonBindings.STRING));
    final Lazy<JsonBinding<PostReq<String, T>>> read = new Lazy<>(() -> PostReq.jsonBinding(JsonBindings.STRING, bindingT));
    final Factory<T> factoryT = bindingT.factory();
    final Factory<CrudReqs<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<CrudReqs<T>>() {
      @Override
      public Factory<CrudReqs<T>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(CrudReqs<T> _value) {
        JsonObject _result = new JsonObject();
        _result.add("create", create.get().toJson(_value.create));
        _result.add("read", read.get().toJson(_value.read));
        return _result;
      }

      @Override
      public CrudReqs<T> fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new CrudReqs<T>(
          JsonBindings.fieldFromJson(_obj, "create", create.get()),
          JsonBindings.fieldFromJson(_obj, "read", read.get())
        );
      }
    };
  }
}
