
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.FileReader;
import java.nio.file.Paths;
import java.nio.file.Path;

import org.adl.runtime.sys.adlast.Module;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.JsonHelpers;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import adl.test2.S1;
import adl.test5.U8;
import adl.test5.U9;
import adl.test5.S;

public class TestMerging {

  @Test
  public void testMergeStructJson() throws Exception {
    JsonElement jv1 = gson.fromJson("{\"x\":42, \"y\":\"foo\"}", JsonElement.class);
    S1 s1 = S1.jsonBinding().fromJson(jv1);
    assertEquals( 42, s1.getX() );
    assertEquals( "foo", s1.getY() );

    JsonElement jv2 = gson.fromJson("{\"x\":11}", JsonElement.class);
    JsonElement jv3 = JsonHelpers.mergeJson(jv1, jv2);
    s1 = S1.jsonBinding().fromJson(jv3);
    assertEquals( 11, s1.getX() );
    assertEquals( "foo", s1.getY() );
  }

  @Test
  public void testMergeUnionJson() throws Exception {
    JsonElement jv1 = gson.fromJson("{\"v1\":{\"f\": 42}}", JsonElement.class);
    U8 u8 = U8.jsonBinding().fromJson(jv1);
    assertEquals( U8.Disc.V1, u8.getDisc() );
    assertEquals( 42, u8.getV1().getF() );

    JsonElement jv2 = gson.fromJson("{\"v2\":89}", JsonElement.class);
    JsonElement jv3 = JsonHelpers.mergeJson(jv1, jv2);
    u8 = U8.jsonBinding().fromJson(jv3);
    assertEquals( U8.Disc.V2, u8.getDisc() );
    assertEquals( 89, u8.getV2() );
  }

  @Test
  public void testMergeNested() throws Exception {
    JsonElement jv1 = gson.fromJson("{\"f1\":{\"v1\": \"aa\"}, \"f2\" : {\"v2\": 131}}", JsonElement.class);
    S s = S.jsonBinding().fromJson(jv1);
    assertEquals(U9.Disc.V1, s.getF1().getDisc());
    assertEquals("aa", s.getF1().getV1());
    assertEquals(U9.Disc.V2, s.getF2().getDisc());
    assertEquals(131, s.getF2().getV2());

    JsonElement jv2 = gson.fromJson("{\"f2\" : {\"v2\": 231}}", JsonElement.class);
    JsonElement jv3 = JsonHelpers.mergeJson(jv1, jv2);
    s = S.jsonBinding().fromJson(jv3);
    assertEquals(U9.Disc.V1, s.getF1().getDisc());
    assertEquals("aa", s.getF1().getV1());
    assertEquals(U9.Disc.V2, s.getF2().getDisc());
    assertEquals(231, s.getF2().getV2());

    jv2 = gson.fromJson("{\"f2\" : \"v3\"}", JsonElement.class);
    jv3 = JsonHelpers.mergeJson(jv1, jv2);
    s = S.jsonBinding().fromJson(jv3);
    assertEquals(U9.Disc.V1, s.getF1().getDisc());
    assertEquals("aa", s.getF1().getV1());
    assertEquals(U9.Disc.V3, s.getF2().getDisc());
  }

  @Test
  public void testMergeEmptyObjects() {
    // Shouldn't crash.
    JsonElement empty = JsonHelpers.mergeJson(new JsonObject(), new JsonObject());
    assertEquals(gson.toJson(empty), "{}");
  }

  static Gson gson = new GsonBuilder()
    .serializeNulls()
    .create();
};
