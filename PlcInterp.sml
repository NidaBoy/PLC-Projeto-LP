exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (e:expr) (env:plcVal env) : plcVal =
	case e of
		  ConI i => IntV i
		| ConB b => BoolV b
		| Var x => lookup env x
		| List [] => ListV []
		| List v => ListV (map (fn exp => eval exp env) v)
		| Prim1(opr, e1) =>
				let
					val v1 = eval e1 env
				in
					case (opr, v1) of
						  ("-", IntV i) => IntV (~i)
						| ("print", _) =>
										let
											val s = val2string v1
										in
											print(s^"\n"); ListV []
										end
						| _   => raise Impossible
				end
		| Prim2(opr, e1, e2) =>
				let
					val v1 = eval e1 env
					val v2 = eval e2 env
				in
					case (opr, v1, v2) of
						  ("*" , IntV i1, IntV i2) => IntV (i1 * i2)
						| ("/" , IntV i1, IntV i2) => IntV (i1 div i2)
						| ("+" , IntV i1, IntV i2) => IntV (i1 + i2)
						| ("-" , IntV i1, IntV i2) => IntV (i1 - i2)
						| (";", _ , _) => v2
						| _ => raise Impossible
				end
		| Let(x, e1, e2) =>
				let
					val v = eval e1 env
					val env2 = (x,v) :: env
				in
					eval e2 env2
				end
		| ESeq (SeqT _) => SeqV []
		| ESeq _ => raise Impossible
		| Letrec(f, xt, x, ft, e1, e2) =>
				let
					val env' = (f, Clos (x, f, e1, env)) :: (x, Clos (x, f, e1, env)) :: env
					val v1 = eval e1 env'
					val v2 = eval e2 env'
				in
					v2
				end
		| If(c, e1, e2) =>
				let
					val vc = eval c env
				in
					case vc of
						  BoolV true => eval e1 env
						| BoolV false => eval e2 env
						| _ => raise Impossible
				end
		| Match(e, cases) =>
				let
					val v = eval e env
				in
					case v of
						  ListV [] => raise HDEmptySeq
						| ListV (hd::tl) =>
								let
									fun matchCase ((pat, exp): expr option * expr) =
										case pat of
											  NONE => eval exp env
											| SOME p =>
												  let
													  val pv = eval p env
												  in
													  if pv = hd then eval exp env
													  else raise MatchResTypeDiff
												  end
								in
									case (List.filter (fn (_,exp) => eval exp env = v) cases) of
								  	    [] => raise NoMatchResults
									  | (pat, exp) :: _ => matchCase (pat, exp)
								end
						| _ => raise Impossible
				end
		| Call(e1, e2) =>
				let
					val v1 = eval e1 env
					val v2 = eval e2 env
				in
					case v1 of
						  Clos (x, f, body, env') =>
							  	let
							  		val env'' = (f, v1) :: (x, v2) :: env'
							  	in
							  		eval body env''
							  	end
						| _ => raise NotAFunc
				end
		| Item(i, e) =>
				let
					val v = eval e env
				in
					case v of
						  ListV l =>
							  	if i < 0 orelse i >= List.length l then raise ValueNotFoundInMatch
							  	else List.nth (l, i)
						| _ => raise Impossible
				end
		| Anon(xt, x, e) =>
				let
					val v = eval e env
				in
					Clos (x, "", e, env)
				end
