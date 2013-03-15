import org.eclipse.imp.pdb.facts.IInteger;
import org.eclipse.imp.pdb.facts.IValueFactory;

public class Sleep {
	public Sleep(IValueFactory values){
		super();
	}
	
	public void sleep(IInteger ms) {
		try {
			Thread.sleep(ms.intValue());
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
