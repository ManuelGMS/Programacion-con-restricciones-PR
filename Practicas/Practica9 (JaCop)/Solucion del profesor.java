package practica10;

import java.lang.reflect.Array;

import org.jacop.constraints.XgtY;
import org.jacop.constraints.XneqY;
import org.jacop.core.IntVar;
import org.jacop.core.Store;
import org.jacop.search.DepthFirstSearch;
import org.jacop.search.IndomainMin;
import org.jacop.search.Search;
import org.jacop.search.SelectChoicePoint;
import org.jacop.search.SimpleSelect;
import org.jacop.search.SmallestDomain;

public class Practica10 {

	final static int numAristas = 5;
	
	//1º
	static Store store= new Store();
	
	//2º
	static IntVar[]ini= new IntVar[numAristas];
	static IntVar[]fin=new IntVar[numAristas];
	
	//3º
	private static void inicializa(){
		
		for(int i = 0; i < numAristas; i++){		
			ini[i] = new IntVar(store,"ini"+i,1,numAristas);
			fin[i] = new IntVar(store,"fin"+i,1,numAristas);
		}	
		
		
	}
	
	//4º ini[i] != fin[i]
	private static void distintosIniFin(){
		
		for(int i = 0; i < numAristas; i++){
			store.impose(new XneqY(ini[i],fin[i]));
		}
	}
	
	//5º ini[i] != ini[j] && fin[i+ != fin[j]
	private static void distintosEntreSi(){
		
		for(int i = 0; i < numAristas; i++){
			for(int j = i+1; j < numAristas; j++){
				store.impose(new XneqY(ini[i],ini[j]));	
				store.impose(new XneqY(fin[i],fin[j]));
			}
		}
	}
	
	//6º
	private static void ordenados(){
		
		for(int i = 0; i < numAristas-1; i++){
			store.impose(new XgtY(fin[i],fin[i+1]));
		}
	}
	
	public static void main(String[] args) {
		
		inicializa();
		distintosIniFin();
		distintosEntreSi();
		ordenados();
		
		Search<IntVar> search = new DepthFirstSearch<IntVar>(); 
		SelectChoicePoint<IntVar> select = new SimpleSelect<IntVar>(concatenate(ini,fin), 
				   										new SmallestDomain<IntVar>(), 
		                                              
		                                               new IndomainMin<IntVar>()); 
		boolean result = search.labeling(store, select);
		System.out.println("Satisfactible: "+result);
		
		
	}
	static public <T> T[] concatenate(T[] a, T[] b) {
	    int aLen = a.length;
	    int bLen = b.length;

	    @SuppressWarnings("unchecked")
	    T[] c = (T[]) Array.newInstance(a.getClass().getComponentType(), aLen + bLen);
	    System.arraycopy(a, 0, c, 0, aLen);
	    System.arraycopy(b, 0, c, aLen, bLen);

	    return c;
	}

}
