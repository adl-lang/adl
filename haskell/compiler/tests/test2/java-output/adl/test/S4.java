package adl.test;

public class S4<T> {

  public S3<String> f1;
  public S3<T> f2;

  public S4(S3<String> f1, S3<T> f2) {
    this.f1 = f1;
    this.f2 = f2;
  }

  public S4(Class<T> classT) {
    try {
      this.f1 = new S3<String>();
      this.f2 = new S3<T>(classT);
    }
    catch (ReflectiveOperationException e) {
      throw new RuntimeException(e);
    }
  }

  public S4(S4<T> other, Class<T> classT) {
    try {
      this.f1 = new S3<String>(other.f1);
      this.f2 = new S3<T>(other.f2,classT);
    }
    catch (ReflectiveOperationException e) {
      throw new RuntimeException(e);
    }
  }
}