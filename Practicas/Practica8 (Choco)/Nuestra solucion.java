import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solution;
import org.chocosolver.solver.variables.IntVar;

/*
 * MANUEL GUERRERO MOÑÚS
 * ALEJANDRO CILLEROS GARRUDO
 * 
 */

public class main {

	public static void main(String[] args) {
	
		
		Model model = new Model("Practica PR");
		final int teams = 6;
		IntVar[][] as = new IntVar[teams][];
		
		// P1 declarar cada array as[0]... hasta as[teams-1]
		// como un array de IntVar con dominio entre 0 y teams-1
		
		for(int i = 0; i < teams; i++) {
			as[i] = model.intVarArray("equipo"+i, teams-1, 0, teams-1);
			model.allDifferent(as[i]).post();
		}
		
		for(int i = 0; i < teams; i++) {
			for (int j = 0; j < teams - 1; j++)
				model.arithm(as[i][j], "!=", i).post();
		}
		
		for(int i = 0; i < teams ; i++) {
			for (int j = 0; j < teams - 1; j++){
				for(int k = 0; k < teams; k++) {
					model.ifThen(model.arithm(as[i][j], "=", k), 
								 model.arithm(as[k][j], "=", i));
				}
			}
		}
		
		Solution solution = model.getSolver().findSolution();
		
		if(solution != null) {
			System.out.println(solution);
		}
	}

}
