module type PAIR = sig
  type ('a, 'b) t = 'a * 'b

  val collate : ('a * 'a -> int) -> ('b * 'b -> int)
                -> ('a, 'b) t * ('a, 'b) t -> int
end

module Pair : PAIR = struct
  type ('a, 'b) t = 'a * 'b

  let collate cmpl cmpr ((x, y), (x', y')) =
    match cmpl (x, x') with
    | 0 -> cmpr (y, y')
    | int -> int
end

module type SUM = sig
  type ('a, 'b) t =
  | Inl of 'a
  | Inr of 'b

  val collate : ('a * 'a -> int) -> ('b * 'b -> int)
                -> ('a, 'b) t * ('a, 'b) t -> int
end

module Sum : SUM = struct
  type ('a, 'b) t =
  | Inl of 'a
  | Inr of 'b

  let collate cmpl cmpr = function
    | (Inl _, Inr _)  -> -1
    | (Inr _, Inl _)  -> 1
    | (Inl x, Inl x') -> cmpl (x, x')
    | (Inr x, Inr x') -> cmpr (x, x')
end

module List = struct
  include List
  let zip (xs,ys) = map2 (fun x y -> (x,y)) xs ys
  let rec collate compare (xs, ys) =
    match (xs, ys) with
    | [], [] -> 0
    | x::xs, y::ys ->
      let c = compare(x,y) in
      if c <> 0 then c else
      collate compare (xs, ys)
    | [], ys -> -1
    | xs, [] -> 1
end

module type ALGORITHM = sig
  type ('s, 'e, 'a) t

  val return : 'a -> ('s, 'e, 'a) t
  val (>>)   : ('s, 'e, 'a) t -> ('s, 'e, 'b) t -> ('s, 'e, 'b) t
  val (>>=)  : ('s, 'e, 'a) t -> ('a -> ('s, 'e, 'b) t) -> ('s, 'e, 'b) t
  val fail   : 'e -> ('s, 'e, 'a) t
  val get    : ('s, 'e, 's) t
  val put    : 's -> ('s, 'e, unit) t

  val run : ('s, 'e, 'a) t -> 's -> ('e, 'a * 's) Sum.t
end

module Algorithm : ALGORITHM = struct
  type ('s, 'e, 'a) t = 's -> ('e, 'a * 's) Sum.t

  let return x s = Sum.Inr (x, s)

  let (>>=) m k s =
    match m s with
    | Sum.Inl e       -> Sum.Inl e
    | Sum.Inr (x, s') -> k x s'

  let (>>) m n = (>>=) m (function _ -> n)

  let fail e _ = Sum.Inl e

  let get s = Sum.Inr (s, s)

  let put s _ = Sum.Inr ((), s)

  let run m = m
end

module type SET = sig
  type elt
  type t

  val empty     : t
  val singleton : elt -> t

  val foldr : (elt * 'a -> 'a) -> 'a -> t -> 'a
  val foldl : (elt * 'a -> 'a) -> 'a -> t -> 'a

  val forall : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool

  val member       : elt -> t -> bool
  val subset       : t -> t -> bool
  val properSubset : t -> t -> bool
  val insert       : elt -> t -> t
  val delete       : elt -> t -> t
  val union        : t -> t -> t
  val inter        : t -> t -> t
  val diff         : t -> t -> t

  val equal  : t * t -> bool
  val compare : t * t -> int

  val toList   : t -> elt list
  val fromList : elt list -> t
end

module type OrderedType = sig
  type elt
  val compare : (elt * elt) -> int
end

module Set(Ord: OrderedType) : SET with type elt = Ord.elt = struct
  type elt = Ord.elt

  type t =
    | EMPTY
    | BRANCH of t * elt * t

  let empty         = EMPTY
  let singleton elt = BRANCH (EMPTY, elt, EMPTY)

  let rec foldr f z = function
    | EMPTY                  -> z
    | BRANCH (lhs, elt, rhs) ->
        foldr f (foldr f (f (elt, z)) rhs) lhs

  let rec foldl f z = function
    | EMPTY                  -> z
    | BRANCH (lhs, elt, rhs) ->
        foldl f (foldl f (f (elt, z)) lhs) rhs

  let forall pred =
    foldl (fun (elt, truth) ->
      truth && pred elt
    ) true

  let exists pred =
    foldl (fun (elt, truth) ->
      truth || pred elt
    ) false

  let rec member elt = function
    | EMPTY                   -> false
    | BRANCH (lhs, elt', rhs) ->
        let c = Ord.compare (elt, elt') in
        if c < 0 then member elt lhs else
        if c = 0 then true
                 else member elt rhs

  let subset s t =
    forall (fun elt ->
      member elt t
    ) s

  let rec insert elt = function
    | EMPTY                   -> singleton elt
    | BRANCH (lhs, elt', rhs) ->
        let c = Ord.compare (elt, elt') in
        if c < 0 then BRANCH (insert elt lhs, elt', rhs) else
        if c = 0 then BRANCH (lhs, elt', rhs)
                 else BRANCH (lhs, elt', insert elt rhs)

  let union s t =
    foldl (fun (elt, s) ->
      insert elt s
    ) s t

  let inter s t =
    foldl (fun (elt, u) ->
      if member elt t
      then insert elt u
      else u
    ) empty s

  let diff s t =
    foldl (fun (elt, u) ->
      if member elt t
      then u
      else insert elt u
    ) empty s

  let delete elt s =
    diff s (singleton elt)

  let toList =
    foldr (fun (elt, l) ->
      elt :: l
    ) []

  let fromList l =
    List.fold_right (fun elt s ->
      insert elt s
    ) l empty

  let compare (s, t) =
    List.collate Ord.compare (toList s, toList t)

  let equal (s, t) =
    compare (s, t) = 0

  let properSubset s t =
    not (equal (s, t)) && subset s t
end

module type MAP = sig
  type key
  type 'a t

  val empty     : 'a t
  val singleton : key -> 'a -> 'a t

  val foldr : (key * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
  val foldl : (key * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
  val map   : ('a -> 'b) -> 'a t -> 'b t
  val mapi  : (key * 'a -> 'b) -> 'a t -> 'b t

  val forall : (key * 'a -> bool) -> 'a t -> bool
  val exists : (key * 'a -> bool) -> 'a t -> bool

  val dom    : 'a t -> key list
  val cod    : 'a t -> 'a list
  val member : key -> 'a t -> bool
  val lookup : key -> 'a t -> 'a option
  val insert : key -> 'a -> 'a t -> 'a t
  val merger : 'a t -> 'a t -> 'a t
  val mergel : 'a t -> 'a t -> 'a t

  val toList : 'a t -> (key * 'a) list
  val fromList : (key * 'a) list -> 'a t

  val collate : ('a * 'a -> int) -> 'a t * 'a t -> int
end

module type OrderedTypeMap = sig
  type key
  val compare : (key * key) -> int
end

module Map(Ord:OrderedTypeMap) : MAP with type key = Ord.key = struct
  type key = Ord.key
  type 'a t = EMPTY | BRANCH of 'a t * key * 'a * 'a t

  let empty             = EMPTY
  let singleton key elt = BRANCH (EMPTY, key, elt, EMPTY)

  let rec foldr f z = function
    | EMPTY                       -> z
    | BRANCH (lhs, key, elt, rhs) ->
        foldr f (foldr f (f (key, elt, z)) rhs) lhs

  let rec foldl f z = function
    | EMPTY                       -> z
    | BRANCH (lhs, key, elt, rhs) ->
        foldl f (foldl f (f (key, elt, z)) lhs) rhs

  let rec map f = function
    | EMPTY                       -> EMPTY
    | BRANCH (lhs, key, elt, rhs) ->
        BRANCH (map f lhs, key, f elt, map f rhs)

  let rec mapi f = function
    | EMPTY                       -> EMPTY
    | BRANCH (lhs, key, elt, rhs) ->
        BRANCH (mapi f lhs, key, f (key, elt), mapi f rhs)

  let forall pred =
    foldl (fun (key, elt, truth) ->
      truth && pred (key, elt)
    ) true

  let exists pred =
    foldl (fun (key, elt, truth) ->
      truth || pred (key, elt)
    ) false

  let dom m =
    foldl (fun (key, _, s) ->
      key :: s
    ) [] m

  let cod m =
    foldl (fun (_, elt, s) ->
      elt :: s
    ) [] m

  let rec member key = function
    | EMPTY                        -> false
    | BRANCH (lhs, key', _, rhs) ->
        let c = Ord.compare (key, key') in
        if c < 0 then member key lhs else
        if c = 0 then true
                 else member key rhs

  let rec lookup key = function
    | EMPTY                        -> None
    | BRANCH (lhs, key', elt, rhs) ->
        let c = Ord.compare (key, key') in
        if c < 0 then lookup key lhs else
        if c = 0 then Some elt
                 else lookup key rhs

  let rec insert key elt = function
    | EMPTY                         -> singleton key elt
    | BRANCH (lhs, key', elt', rhs) ->
        let c = Ord.compare (key, key') in
        if c < 0 then BRANCH (insert key elt lhs, key', elt', rhs) else
        if c = 0 then BRANCH (lhs, key, elt, rhs)
                 else BRANCH (lhs, key', elt', insert key elt rhs)

  let merger s t =
    foldl (fun (key, elt, s) ->
      insert key elt s
    ) s t

  let mergel s t =
    foldl (fun (key, elt, t) ->
      insert key elt t
    ) t s

  let toList s =
    foldr (fun (key, elt, l) ->
      (key, elt) :: l
    ) [] s

  let fromList l =
    List.fold_right (fun (key, elt) s ->
      insert key elt s
    ) l empty

  let collate cmp (s, t) =
    List.collate (fun ((key, elt), (key', elt')) ->
        match Ord.compare (key, key') with
        | 0 -> cmp (elt, elt')
        | int -> int
    ) (toList s, toList t)
end

module type REGION_INFERENCE_ALGORITHM = sig
  type ident = string

  type tyVar
  type regVar
  type regVarSet
  type effVar
  type effect
  type annotatedType

  module MLType : sig
    type t =
      | Int
      | TyVar of tyVar
      | Arrow of t * t
  end

  module MLTypeScheme : sig
    type t =
    | MLType of MLType.t
    | Forall of tyVar * t
  end

  module TypedExp : sig
    type t =
    | Var    of ident * MLType.t list
    | Abs    of ident * MLType.t * t
    | App    of t * t
    | Letrec of ident * MLTypeScheme.t * ident * t * t
  end

  module RegionExp : sig
    type 'a t =
    | VarX      of ident * 'a
    | VarF      of ident * regVar list * regVar * 'a
    | Abs       of ident * 'a t * regVar * 'a
    | App       of 'a t * 'a t * 'a
    | Letrec    of ident * regVar list * ident * regVar * 'a t * 'a t * 'a
    | Letregion of regVarSet * 'a t * 'a

    val fmt : 'a t -> string
  end

  val infer : TypedExp.t -> ((annotatedType * regVar) * effect) RegionExp.t option
end

module RegionInferenceAlgorithm : REGION_INFERENCE_ALGORITHM = struct
  type ident = string

  type err =
  | TyApp
  | NotConsistent
  | NotDisjoint
  | NotInScope of ident
  | Cyclic
  | Unexpected
  | XXX

  module IdentSet = Set(struct
    type elt = ident
    let compare = fun (a,b) -> String.compare a b
  end)

  module IdentMap = Map(struct
    type key = ident
    let compare = fun (a,b) -> String.compare a b
  end)

  module TyVar : sig
    type t
    val intro   : ident -> t
    val elim    : t -> ident
    val equal   : t * t -> bool
    val compare : t * t -> int
  end = struct
    type t = ident
    let intro x = x
    let elim x = x
    let equal (x, x') = x = x'
    let compare = fun (a,b) -> String.compare a b
  end

  module TyVarSet = Set(struct
    type elt = TyVar.t
    let compare = TyVar.compare
  end)

  module TyVarMap = Map(struct
    type key = TyVar.t
    let compare = TyVar.compare
  end)

  module MLType = struct
    type t =
    | Int
    | TyVar of TyVar.t
    | Arrow of t * t
  end

  module MLTypeScheme = struct
    type t =
    | MLType of MLType.t
    | Forall of TyVar.t * t
  end

  module TypedExp = struct
    type t =
    | Var    of ident * MLType.t list
    | Abs    of ident * MLType.t * t
    | App    of t * t
    | Letrec of ident * MLTypeScheme.t * ident * t * t
  end

  module RegVar : sig
    type t
    val intro   : ident -> t
    val elim    : t -> ident
    val equal   : t * t -> bool
    val compare : t * t -> int
  end = struct
    type t = ident
    let intro x = x
    let elim x = x
    let equal (x, x') = x = x'
    let compare = fun (a,b) -> String.compare a b
  end

  module RegVarSet = Set(struct
    type elt = RegVar.t
    let compare = RegVar.compare
  end)

  module RegVarMap = Map(struct
    type key = RegVar.t
    let compare = RegVar.compare
  end)

  module RegionExp = struct

    type 'a t =
    | VarX      of ident * 'a
    | VarF      of ident * RegVar.t list * RegVar.t * 'a
    | Abs       of ident * 'a t * RegVar.t * 'a
    | App       of 'a t * 'a t * 'a
    | Letrec    of ident * RegVar.t list * ident * RegVar.t * 'a t * 'a t * 'a
    | Letregion of RegVarSet.t * 'a t * 'a

    let rec fmt = function
      | (VarX (x, _)) -> x
      | (VarF (x, _, _, _)) -> x
      | (Abs (x, e, r, _)) -> "(^" ^ x ^ "." ^ fmt e ^ " at " ^ RegVar.elim r ^ ")"
      | (App (e1, e2, _)) -> "(" ^ fmt e1 ^ " " ^ fmt e2 ^ ")"
      | (Letrec _) -> ""
      | (Letregion (s, e, _)) ->
          "(letregion " ^
          RegVarSet.foldl (fun (r, s) ->
            RegVar.elim r ^ " " ^ s
          ) "" s ^
          fmt e ^ ")"

    let uof = function
    | VarX      (_, (u, _))                -> u
    | VarF      (_, _, _, (u, _))          -> u
    | Abs       (_, _, _, (u, _))          -> u
    | App       (_, _, (u, _))             -> u
    | Letrec    (_, _, _, _, _, _, (u, _)) -> u
    | Letregion (_, _, (u, _))             -> u

    let pof = function
    | VarX      (_, (_, p))                -> p
    | VarF      (_, _, _, (_, p))          -> p
    | Abs       (_, _, _, (_, p))          -> p
    | App       (_, _, (_, p))             -> p
    | Letrec    (_, _, _, _, _, _, (_, p)) -> p
    | Letregion (_, _, (_, p))             -> p

    let get = function
    | VarX      (_, x)                -> x
    | VarF      (_, _, _, x)          -> x
    | Abs       (_, _, _, x)          -> x
    | App       (_, _, x)             -> x
    | Letrec    (_, _, _, _, _, _, x) -> x
    | Letregion (_, _, x)             -> x

    let set x = function
    | VarX      (a, _)                -> VarX (a, x)
    | VarF      (a, b, c, _)          -> VarF (a, b, c, x)
    | Abs       (a, b, c, _)          -> Abs (a, b, c, x)
    | App       (a, b, _)             -> App (a, b, x)
    | Letrec    (a, b, c, d, e, f, _) -> Letrec (a, b, c, d, e, f, x)
    | Letregion (a, b, _)             -> Letregion (a, b, x)
  end

  module EffVar : sig
    type t
    val intro   : ident -> t
    val elim    : t -> ident
    val equal   : t * t -> bool
    val compare : t * t -> int
  end = struct
    type t = ident
    let intro x = x
    let elim x = x
    let equal (x, x') = x = x'
    let compare = fun (a,b) -> String.compare a b
  end

  module EffVarSet = Set(struct
    type elt = EffVar.t
    let compare = EffVar.compare
  end)

  module EffVarMap = Map(struct
    type key = EffVar.t
    let compare = EffVar.compare
  end)

  module AtEff = struct
    type t = (RegVar.t, EffVar.t) Sum.t
    let compare = Sum.collate RegVar.compare EffVar.compare
  end

  module Effect = Set(struct
    type elt = AtEff.t
    let compare = AtEff.compare
  end)

  module EffectMap = Map(struct
    type key = EffVar.t
    let compare = EffVar.compare
  end)

  module ArrEff = struct
    type t = EffVar.t * Effect.t
    let compare = Pair.collate EffVar.compare Effect.compare
    let equal x = compare x = 0

    let frv (_, phi) =
      Effect.foldl (function
        | (Sum.Inl r, s) -> RegVarSet.insert r s
        | (Sum.Inr _, s) -> s
      ) RegVarSet.empty phi

    let fev (e, phi) =
      EffVarSet.union
        (EffVarSet.singleton e)
        (Effect.foldl (function
            | (Sum.Inl _, s) -> s
            | (Sum.Inr e, s) -> EffVarSet.insert e s
        ) EffVarSet.empty phi)
  end

  module ArrEffSet1 = Set(struct
    type elt = ArrEff.t
    let compare = ArrEff.compare
  end)

  module ArrEffSet = struct
    include ArrEffSet1

    let functional s =
      forall (fun (e, p) ->
          forall (fun (e', p') ->
              not (EffVar.equal (e, e')) || (Effect.equal (p, p'))
          ) s
      ) s

    let closed s =
      forall (fun (_, p) ->
        Effect.forall (function
          | Sum.Inl _ -> true
          | Sum.Inr e' ->
            exists (fun (e'', _) ->
              EffVar.equal (e', e'')
            ) s
        ) p
      ) s

    let transitive s =
      forall (fun (e1, p1) ->
        forall (fun (e2, p2) ->
            not (Effect.member (Sum.Inr e2) p1)
            || Effect.subset p2 p1
        ) s
      ) s

    let fev s =
      foldl (fun (e, t) ->
        EffVarSet.union (ArrEff.fev e) t
      ) EffVarSet.empty s

    let frv s =
      foldl (fun (e, t) ->
        RegVarSet.union (ArrEff.frv e) t
      ) RegVarSet.empty s

    let effectMap s =
      if functional s then
        Some (
          foldl (fun ((e, p), m) ->
            EffectMap.insert e p m
          ) EffectMap.empty s)
      else
        None
  end

  module FV = Set(struct
    type elt = (TyVar.t, (RegVar.t, EffVar.t) Sum.t) Sum.t
    let compare =
      Sum.collate TyVar.compare (Sum.collate RegVar.compare EffVar.compare)
  end)

  module AnnotatedType = struct
    type t =
    | Int
    | TyVar of TyVar.t
    | Arrow of (t * RegVar.t) * ArrEff.t * (t * RegVar.t)

    let rec ml = function
      | TyVar a                    -> MLType.TyVar a
      | Int                        -> MLType.Int
      | Arrow ((t, _), _, (t', _)) -> MLType.Arrow (ml t, ml t')

    let rec arreff = function
      | TyVar a                    -> ArrEffSet.empty
      | Int                        -> ArrEffSet.empty
      | Arrow ((t, _), e, (t', _)) ->
        ArrEffSet.(union (singleton e) (union (arreff t) (arreff t')))

    let rec ftv = TyVarSet.(function
      | TyVar a                    -> singleton a
      | Int                        -> empty
      | Arrow ((t, _), _, (t', _)) -> union (ftv t) (ftv t')
    )

    let rec fev = EffVarSet.(function
      | TyVar _                    -> empty
      | Int                        -> empty
      | Arrow ((t, _), e, (t', _)) ->
          union (ArrEff.fev e) (union (fev t) (fev t'))
    )

    let rec frv = RegVarSet.(function
      | TyVar _                     -> empty
      | Int                         -> empty
      | Arrow ((t, r), e, (t', r')) ->
          union
            (singleton r)
            (union
              (singleton r')
              (union
                (ArrEff.frv e)
                (union
                  (frv t)
                  (frv t'))))
    )

    let fv t = FV.(
      let ftv =
          TyVarSet.foldl (fun (a, s) ->
            insert (Sum.Inl a) s
          ) empty (ftv t)
      in
      let frv =
        RegVarSet.foldl (fun (a, s) ->
          insert (Sum.Inr (Sum.Inl a)) s
        ) empty (frv t)
      in
      let fev =
        EffVarSet.foldl (fun (a, s) ->
          insert (Sum.Inr (Sum.Inr a)) s
        ) empty (fev t)
      in
      union ftv (union frv fev)
    )

    let substRho (st, sr, se) r =
      match RegVarMap.lookup r sr with
      | None -> r
      | Some r -> r

    let substEffect ((st, sr, se) as s) p =
      Effect.foldl (function
        | (Sum.Inl r, t) -> Effect.insert (Sum.Inl (substRho s r)) t
        | (Sum.Inr e, t) ->
            let (e', p') = match EffVarMap.lookup e se with
              | None -> (e, Effect.empty)
              | Some (e', p') -> (e', p')
            in
            Effect.union (Effect.insert (Sum.Inr e') p') t
            
      ) Effect.empty p

    let substArrEff ((st, sr, se) as s) (e, p) =
      let (e', p') = match EffVarMap.lookup e se with
        | None -> (e, Effect.empty)
        | Some (e', p') -> (e', p')
      in
      (e', Effect.union p' (substEffect s p))

    let rec subst ((st, sr, se) as s) = function
       | Int                         -> Int
       | TyVar a                     ->
           (match TyVarMap.lookup a st with
           | None -> TyVar a
           | Some t -> t)
       | Arrow ((t, r), e, (t', r')) ->
           Arrow
             ( (subst s t, substRho s r)
             , substArrEff s e
             , (subst s t', substRho s r'))


    let substWithPlace s (t, r) = (subst s t, substRho s r)

    let compose ((st, sr, se) as s) (st', sr', se') =
    let st'' = TyVarMap.map (subst s) st' in
    let sr'' = RegVarMap.map (substRho s) sr' in
    let se'' = EffVarMap.map (substArrEff s) se' in
    ( TyVarMap.merger st st''
    , RegVarMap.merger sr sr''
    , EffVarMap.merger se se'' )

    let suppt st =
      TyVarSet.foldl (fun (a, s) ->
        if not (TyVar.equal (st a, a)) then
          TyVarSet.insert a s
        else
          s
      ) TyVarSet.empty

    let suppr sr =
      RegVarSet.foldl (fun (r, s) ->
        if not (RegVar.equal (sr r, r)) then
          RegVarSet.insert r s
        else
          s
      ) RegVarSet.empty

    let suppe se =
      EffVarSet.foldl (fun (e, s) ->
        if not (ArrEff.equal (se e, (e, Effect.empty))) then
          EffVarSet.insert e s
        else
          s
      ) EffVarSet.empty

    let supp (st, sr, se) (vt, vr, ve) = FV.(
      let suppt =
        TyVarSet.foldl (fun (a, s) ->
          insert (Sum.Inl a) s
        ) empty (suppt st vt)
      in
      let suppr =
        RegVarSet.foldl (fun (a, s) ->
          insert (Sum.Inr (Sum.Inl a)) s
        ) empty (suppr sr vr)
      in
      let suppe =
        EffVarSet.foldl (fun (a, s) ->
          insert (Sum.Inr (Sum.Inr a)) s
        ) empty (suppe se ve)
      in
      union suppt (union suppr suppe)
    )

    let idt = TyVarMap.empty
    let idr = RegVarMap.empty
    let ide = EffVarMap.empty
    let id  = (idt, idr, ide)

    open Algorithm

    let unifyRho (r:RegVar.t) (r':RegVar.t) =
      if RegVar.equal (r, r') then
        return id
      else
        return (idt, RegVarMap.singleton r r', ide)

    let unifyArrEff (e, eff) (e', eff') =
      if EffVar.equal (e, e') then
        return id
      else(
        let eff'' = Effect.union eff eff' in
        return
          ( idt
          , idr
          , EffVarMap.fromList [(e, (e', eff'')); (e', (e', eff''))] ))

    let rec unifyMu (t, r) (t', r') =
      unifyRho r r' >>= fun s1 ->
      match (t, t') with
      | (TyVar x, _) ->
          if TyVarSet.member x (ftv t') then
            fail Cyclic
          else
            return (TyVarMap.singleton x t', idr, ide)
      | (_, TyVar x) ->
          if TyVarSet.member x (ftv t) then
            fail Cyclic
          else
            return (TyVarMap.singleton x t, idr, ide)
      | (Int, Int) -> return s1
      | (Arrow (u1, e, u2), Arrow (u1', e', u2')) ->
          unifyMu u1 u1' >>= fun s2 ->
          unifyMu u2 u2' >>= fun s3 ->
          unifyArrEff e e' >>= fun s4 ->
          return (compose s4 (compose s3 (compose s2 s1)))
      | (_, _) -> fail Unexpected
  end

  module AnnotatedTypeScheme = struct
    type t = TyVar.t list * RegVar.t list * EffVar.t list * AnnotatedType.t
  end

  module Basis = struct
    type t = RegVarSet.t *  ArrEffSet.t

    let empty = (RegVarSet.empty, ArrEffSet.empty)

    let subst s (q, p) =
      let q' = List.map (AnnotatedType.substRho s) (RegVarSet.toList q) in
      let p' = List.map (AnnotatedType.substArrEff s) (ArrEffSet.toList p) in
      (RegVarSet.fromList q', ArrEffSet.fromList p')

    let dom (q, p) =
        match ArrEffSet.effectMap p with
        | None   -> None
        | Some m ->
            Some
              (Effect.union
                (Effect.fromList (List.map (fun v -> Sum.Inl v) (RegVarSet.toList q)))
                (Effect.fromList (List.map (fun v -> Sum.Inr v) (EffectMap.dom m))))

    let consistent (q, p) = RegVarSet.subset (ArrEffSet.frv p) q

    let frv (q, p) = RegVarSet.union q (ArrEffSet.frv p)
    let fev (_, p) = ArrEffSet.fev p

    let union (q1, p1) (q2, p2) =
        (RegVarSet.union q1 q2, ArrEffSet.union p1 p2)

    let diff (q1, p1) (q2, p2) =
        (RegVarSet.diff q1 q2, ArrEffSet.diff p1 p2)

    let disjointUnion ((q1, p1) as b1) ((q2, p2) as b2) =
        match (ArrEffSet.effectMap p1, ArrEffSet.effectMap p2) with
        | (None, _)                -> None
        | (_, None)                -> None
        | (Some m1, Some m2) ->
            let b3 = union b1 b2 in
            let q3 () = RegVarSet.inter q1 q2 in
            let domM1 () = EffVarSet.fromList (EffectMap.dom m1) in
            let domM2 () = EffVarSet.fromList (EffectMap.dom m2) in
            let domM3 () = EffVarSet.inter (domM1 ()) (domM2 ()) in
            if consistent b3
                && RegVarSet.equal (q3 (), RegVarSet.empty)
                && EffVarSet.equal (domM3 (), EffVarSet.empty)
            then
              Some b3
            else
              None
  end

  let observe b p =
    Effect.inter
      p
      (Effect.union
        (Effect.fromList (List.map (fun v -> Sum.Inl v) (RegVarSet.toList (Basis.frv b))))
        (Effect.fromList (List.map (fun v -> Sum.Inr v) (EffVarSet.toList (Basis.fev b)))))

  let bellow b b0 =
    let (q, p) = b in
    let (q0, p0) = b0 in
    let p1 = ArrEffSet.inter p p0 in
    let q1 = RegVarSet.union q0 (ArrEffSet.frv p1) in
    (q1, p1)
    
  let bellow_t b (b0, t) =
    bellow b (Basis.union b0 (AnnotatedType.frv t, AnnotatedType.arreff t))

  let bellow_u b (b0, (t, r)) =
    bellow
      b
      (Basis.union
        b0
        ( RegVarSet.insert
            r
            (AnnotatedType.frv t)
        , AnnotatedType.arreff t))

  type 'a stream = STREAM of (unit -> 'a * 'a stream)
  type tyVar  = TyVar.t
  type regVar = RegVar.t
  type effVar = EffVar.t
  type regVarSet = RegVarSet.t
  type effect = Effect.t
  type annotatedType = AnnotatedType.t

  let gensym (STREAM s) = s ()

  let rec tyVarStream' i =
    STREAM (fun () ->
      (TyVar.intro ("a" ^ string_of_int i), tyVarStream' (i + 1)))

  let tyVarStream = tyVarStream' 0

  let rec regVarStream' i =
    STREAM (fun () ->
      (RegVar.intro ("r" ^ string_of_int i), regVarStream' (i + 1)))

  let regVarStream = regVarStream' 0

  let rec effVarStream' i =
    STREAM (fun () ->
      (EffVar.intro ("e" ^ string_of_int i), effVarStream' (i + 1)))

  let effVarStream = effVarStream' 0

  open Algorithm

  let getRegVarStream () =
    get >>= fun (regVarStream, _) ->
    return regVarStream

  let putRegVarStream regVarStream =
    get >>= fun (_, effVarStream) ->
    put (regVarStream, effVarStream)

  let getEffVarStream () =
    get >>= fun (_, effVarStream) ->
    return effVarStream

  let putEffVarStream effVarStream =
    get >>= fun (regVarStream, _) ->
    put (regVarStream, effVarStream)

  let freshRegVar b =
    getRegVarStream () >>= fun regVarStream ->
    let (r, regVarStream') = gensym regVarStream in
    putRegVarStream regVarStream' >> return r
    
  let freshEffVar b =
    getEffVarStream () >>= fun effVarStream ->
    let (r, effVarStream') = gensym effVarStream in
    putEffVarStream effVarStream' >> return r

  let rec freshType = function
    | (b, MLType.TyVar a)       -> return (b, AnnotatedType.TyVar a)
    | (b, MLType.Int)           -> return (b, AnnotatedType.Int)
    | (b, MLType.Arrow (t, t')) ->
      freshRegVar b     >>= fun r ->
      freshRegVar b     >>= fun r' ->
      freshEffVar b     >>= fun e ->
      freshType (b, t)  >>= fun (b, t) ->
      freshType (b, t') >>= fun (b, t') ->
      return (b, AnnotatedType.Arrow ((t, r), (e, Effect.empty), (t', r')))

  let rec freshTypeN = function
    | (b, [])      -> return (b, [])
    | (b, t :: ts) ->
        freshType (b, t) >>= fun (b, t) ->
        freshTypeN (b, ts) >>= fun (b, ts) ->
        return (b, t :: ts)

  let freshTypeWithPlace (b, t) =
    freshRegVar b >>= fun r ->
    freshType (b, t) >>= fun (b, t) ->
    return (b, (t, r))

  let newInstance ((q, p), (an, rs, es, t), tn) =
    let n = List.length tn in
    let n' = List.length an in
    if n <> n' then fail TyApp else
    match ArrEffSet.effectMap p with
    | None   -> fail NotConsistent
    | Some m ->
      let rs = RegVarSet.diff (RegVarSet.fromList rs) q in
      let es =
        EffVarSet.diff
          (EffVarSet.fromList es)
          (EffVarSet.fromList (EffectMap.dom m))
      in
      let b'q =
        RegVarSet.foldl (fun (r, b'q) ->
          Effect.insert (Sum.Inl r) b'q
        ) Effect.empty q
      in
      let b'q =
        RegVarSet.foldl (fun (r, b'q) ->
          Effect.insert (Sum.Inl r) b'q
        ) b'q rs
      in
      let b'p =
        ArrEffSet.union
          p
          (EffVarSet.foldl (fun (e, b'p) ->
            ArrEffSet.insert (e, Effect.empty) b'p
          ) ArrEffSet.empty es)
      in
      let b' = (b'q, b'p) in
      freshTypeN (b', tn) >>= fun (b', tn) ->
      let st = TyVarMap.fromList (List.zip (an, tn))
      in
      let sr =
        RegVarMap.fromList (List.zip
          (RegVarSet.toList rs, RegVarSet.toList rs))
      in
      let se =
        EffVarMap.fromList
          (List.zip
            ( EffVarSet.toList es
            , (List.map
                (fun e -> (e, Effect.empty))
                (EffVarSet.toList es))))
      in
      return
        ( (st, sr, se)
        , b'
        , AnnotatedType.subst (st, sr, se) t
        , List.map (AnnotatedType.subst (st, sr, se)) tn)

  let rec retract s a b1 b0 e =
    let u = RegionExp.uof e in
    let p = RegionExp.pof e in
    let bkeep = bellow_u b1 (b0, u) in
    let p' = observe bkeep p in
    let e = RegionExp.set (u, p) e in
    let e =
      if RegVarSet.equal (RegVarSet.empty, (RegVarSet.diff (fst b1) (fst bkeep)))
      then
        RegionExp.set (u, p') e
      else
        RegionExp.Letregion
          (RegVarSet.diff (fst b1) (fst bkeep), e, (u, p'))
    in
    return (s, a, bkeep, e)

  let rec infer a b te e =
    match e with
    | TypedExp.Var (x, ts) ->
        (match IdentMap.lookup x te with
        | None -> fail (NotInScope x)
        | Some t ->
            return
              ( AnnotatedType.id
              , a
              , b
              , RegionExp.VarX (x, (t, Effect.empty)) ))
    | TypedExp.Abs (x, t0, e1) ->
        freshTypeWithPlace (a, t0) >>= fun (a0, u0) ->
        let a0_diff_a = Basis.diff a0 a in
        (match Basis.disjointUnion b a0_diff_a with
        | None -> fail NotDisjoint
        | Some b_u_a0_diff_a ->
            infer a0 b_u_a0_diff_a (IdentMap.insert x u0 te) e1
            >>= fun (s1, a1, b1, e1) ->
            let u1 = RegionExp.uof e1 in
            let p1 = RegionExp.pof e1 in
            freshEffVar a1 >>= fun e ->
            freshRegVar a1 >>= fun r ->
            let a2 =
                ( RegVarSet.singleton r
                , ArrEffSet.singleton (e, p1))
            in
            let s =
               AnnotatedType.compose
                 ( AnnotatedType.idt
                 , AnnotatedType.idr
                 , EffVarMap.singleton e (e, p1) )
                 s1
            in
            match
              Basis.disjointUnion
                a1
                ( RegVarSet.singleton r
                , ArrEffSet.singleton (e, Effect.empty) )
            with
            | None -> fail NotConsistent
            | Some a1' ->
                match Basis.disjointUnion b1 a2 with
                | None -> fail NotConsistent
                | Some b1_u_a2 ->
                    let e =
                      RegionExp.Abs
                        ( x
                        , e1
                        , r
                        , ( ( AnnotatedType.Arrow
                                ( AnnotatedType.substWithPlace s1 u0
                                , (e, p1)
                                , u1 )
                            , r )
                          , Effect.singleton (Sum.Inl r))
                        )
                    in
                    return (s, a1', b1_u_a2, e)
                
        )
    | TypedExp.App (e1, e2) ->
        infer a b te e1 >>= fun (s1, a1, b1, t1) ->
        infer a1 b1 (IdentMap.map (AnnotatedType.substWithPlace s1) te) e2
        >>= fun (s2, a2, b2, t2) ->
        (match RegionExp.uof t1 with
        | (AnnotatedType.Arrow (u2, (e, p0), u1), r0) ->
          let u2' = RegionExp.uof t2 in
          AnnotatedType.unifyMu (AnnotatedType.substWithPlace s2 u2) u2' >>= (fun s3 ->
          let s = AnnotatedType.compose s3 (AnnotatedType.compose s2 s1) in
          let t1 =
            RegionExp.set
             (AnnotatedType.substWithPlace
               (AnnotatedType.compose s3 s2)
               (RegionExp.uof t1)
             , AnnotatedType.substEffect (AnnotatedType.compose s3 s2) (RegionExp.pof t1))
             t1
          in
          let t2 =
            RegionExp.set
              (AnnotatedType.substWithPlace
                s3
                (RegionExp.uof t2)
             , AnnotatedType.substEffect s3 (RegionExp.pof t2))
              t2
          in
          let u1 =
            AnnotatedType.substWithPlace
              (AnnotatedType.compose s3 s2)
              u1
          in
          let p1 = RegionExp.pof t1 in
          let p2 = RegionExp.pof t2 in
          let p3 =
            Effect.union
              (Effect.fromList [Sum.Inr e; Sum.Inl r0])
              (Effect.union
                p0
                (Effect.union p1 p2))
          in
          retract
            s
            a2
            (Basis.subst s3 b2)
            (Basis.subst s b)
            (RegionExp.App (t1, t2, (u1, p3)))
          )
        | _ -> fail Unexpected)
    | TypedExp.Letrec _ -> fail XXX

  let infer = function t ->
    match Algorithm.run
           (infer Basis.empty Basis.empty IdentMap.empty t)
           (regVarStream, effVarStream) with
    | Sum.Inr ((_, _, _, x), _) -> Some x
    | Sum.Inl e -> None
end
