(* PlcChecker *)
exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

fun teval (e:expr) (env: plcType env) : plcType =
	case e of
		  ConI _ => IntT
		| ConB _ => BoolT
		| Var x => lookup env x
		| List [] => ListT []
		| List e => ListT (map (fn aux => teval aux env) e)
		| ESeq (SeqT plcType) => SeqT plcType
		| ESeq _ => raise EmptySeq
		| Prim1(opr, e1) =>
				let
					val t1 = teval e1 env
				in
					case (opr, t1) of
						 ("print", _) => ListT []
						| ("-", IntT) => IntT
						| ("hd", SeqT e) => e
						| ("tl", SeqT e) => SeqT e
						| ("ise", Seqt) => BoolT
						| ("!", BoolT) => BoolT
						| _ => raise UnknownType
				end
		| Prim2(opr, e1, e2) =>
				let
					val t1 = teval e1 env
					val t2 = teval e2 env
				in
					case (opr, t1, t2) of
					  ("*" , IntT, IntT) => IntT
					| ("/" , IntT, IntT) => IntT
					| ("+" , IntT, IntT) => IntT
					| ("-" , IntT, IntT) => IntT
					| (";" , _ , _)    => t2
					| ("<", IntT, IntT) => BoolT
					| ("<=", IntT, IntT) => BoolT
					| ("=", _, _) => if t1 = t2 then BoolT else raise NotEqTypes
					| ("!=", _, _) => if t1 = t2 then BoolT else raise NotEqTypes
					| ("&&", BoolT, BoolT) => BoolT
					| ("::", _, _) => if SeqT t1 = t2 then t2 else raise UnknownType
					| _   =>  raise UnknownType
				end
		| If(c, e1, e2) =>
				let
					val tc = teval c env
					val t1 = teval e1 env
					val t2 = teval e2 env
				in
					if tc <> BoolT then raise IfCondNotBool 
					else (if t1 = t2 then t1 else raise DiffBrTypes)
				end
		| Match(e, cases) =>
				let
					val t = teval e env
				in
					case t of
						  ListT et =>
						  	  let
								  fun matchCase ((pat, exp): expr option * expr) =
								  	  case pat of
									  	    NONE => teval exp env
										  | SOME p =>
										  	  let
											  	  val pt = teval p env
											  in
											  	  if pt = ListT et then teval exp env
												  else raise MatchResTypeDiff
											  end
							  in
							  	  case (List.filter (fn (_,exp) => teval exp env = t) cases) of
								  	    [] => raise NoMatchResults
									  | (pat, exp) :: _ => matchCase (pat, exp)
							  end
						| _ => raise MatchCondTypesDiff
				end
		| Call(e1, e2) =>
				let
					val t1 = teval e1 env
					val t2 = teval e2 env
				in 
					case t1 of
						  FunT(xt, ft) => if xt = t2 then ft else raise CallTypeMisM
						| _ => raise NotFunc
				end	
		| Item(i, e) =>
				let 
					val t = teval e env
				in
					case t of
						  ListT l => 
						  	if i >= 0 andalso i < List.length l then List.nth (l, i)
							else raise ListOutOfRange
						| _ => raise OpNonList
				end
		| Let(x, e1, e2) =>
				let
					val t = teval e1 env
					val env' = (x,t)::env
				in
					teval e2 env'
				end
		| Letrec(f, xt, x, ft, e1, e2) =>
				let
					val env' = (f, FunT (xt, ft))::(x, xt)::env
					val t1 = teval e1 env'
					val t2 = teval e2 env'
				in
					if t1 = ft then t2 else raise WrongRetType
				end
		| Anon(xt, x, e) =>
				let
					val env' = (x, xt)::env
				in
					teval e env'
				end