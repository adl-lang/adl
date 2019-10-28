/* @generated from adl module test24 */

package adl.test24;

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

public class PostReq<I, O> {

  /* Members */

  private String path;
  private TypeToken<I> reqBodyType;
  private TypeToken<O> respType;

  /* Constructors */

  public PostReq(String path, TypeToken<I> reqBodyType, TypeToken<O> respType) {
    this.path = Objects.requireNonNull(path);
    this.reqBodyType = Objects.requireNonNull(reqBodyType);
    this.respType = Objects.requireNonNull(respType);
  }

  /* Accessors and mutators */

  public String getPath() {
    return path;
  }

  public void setPath(String path) {
    this.path = Objects.requireNonNull(path);
  }

  public TypeToken<I> getReqBodyType() {
    return reqBodyType;
  }

  public void setReqBodyType(TypeToken<I> reqBodyType) {
    this.reqBodyType = Objects.requireNonNull(reqBodyType);
  }

  public TypeToken<O> getRespType() {
    return respType;
  }

  public void setRespType(TypeToken<O> respType) {
    this.respType = Objects.requireNonNull(respType);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof PostReq)) {
      return false;
    }
    PostReq<?, ?> other = (PostReq<?, ?>) other0;
    return
      path.equals(other.path) &&
      reqBodyType.equals(other.reqBodyType) &&
      respType.equals(other.respType);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + path.hashCode();
    _result = _result * 37 + reqBodyType.hashCode();
    _result = _result * 37 + respType.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static <I, O> Factory<PostReq<I, O>> factory(Factory<I> factoryI, Factory<O> factoryO) {
    return new Factory<PostReq<I, O>>() {
      final Lazy<Factory<String>> path = new Lazy<>(() -> Factories.STRING);
      final Lazy<Factory<TypeToken<I>>> reqBodyType = new Lazy<>(() -> Factories.typeProxy(factoryI));
      final Lazy<Factory<TypeToken<O>>> respType = new Lazy<>(() -> Factories.typeProxy(factoryO));

      @Override
      public PostReq<I, O> create() {
        return new PostReq<I, O>(
          path.get().create(),
          new TypeToken<I>(factoryI.jsonBinding()),
          new TypeToken<O>(factoryO.jsonBinding())
          );
      }

      @Override
      public PostReq<I, O> create(PostReq<I, O> other) {
        return new PostReq<I, O>(
          other.getPath(),
          other.getReqBodyType(),
          other.getRespType()
          );
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("test24", "PostReq");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryI.typeExpr());
        params.add(factoryO.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }

      @Override
      public JsonBinding<PostReq<I, O>> jsonBinding() {
        return PostReq.jsonBinding(factoryI.jsonBinding(), factoryO.jsonBinding());
      }
    };
  }

  /* Json serialization */

  public static<I, O> JsonBinding<PostReq<I, O>> jsonBinding(JsonBinding<I> bindingI, JsonBinding<O> bindingO) {
    final Lazy<JsonBinding<String>> path = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<TypeToken<I>>> reqBodyType = new Lazy<>(() -> JsonBindings.typeProxy(bindingI));
    final Lazy<JsonBinding<TypeToken<O>>> respType = new Lazy<>(() -> JsonBindings.typeProxy(bindingO));
    final Factory<I> factoryI = bindingI.factory();
    final Factory<O> factoryO = bindingO.factory();
    final Factory<PostReq<I, O>> _factory = factory(bindingI.factory(), bindingO.factory());

    return new JsonBinding<PostReq<I, O>>() {
      @Override
      public Factory<PostReq<I, O>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(PostReq<I, O> _value) {
        JsonObject _result = new JsonObject();
        _result.add("path", path.get().toJson(_value.path));
        _result.add("reqBodyType", reqBodyType.get().toJson(_value.reqBodyType));
        _result.add("respType", respType.get().toJson(_value.respType));
        return _result;
      }

      @Override
      public PostReq<I, O> fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new PostReq<I, O>(
          JsonBindings.fieldFromJson(_obj, "path", path.get()),
          _obj.has("reqBodyType") ? JsonBindings.fieldFromJson(_obj, "reqBodyType", reqBodyType.get()) : new TypeToken<I>(bindingI),
          _obj.has("respType") ? JsonBindings.fieldFromJson(_obj, "respType", respType.get()) : new TypeToken<O>(bindingO)
        );
      }
    };
  }
}
