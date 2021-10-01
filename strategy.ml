type forme = { coord : int * int ; format : int * int ; mutable indice : int } ;;
let forme_vide = { coord = (-1,-1) ; format = (-1,-1) ; indice = -1 } in

let compteur_sabotage = ref 0 in
let opponentcount = int_of_string (input_line stdin) in
let pos = [|(0,0);(0,0);(0,0);(0,0)|] in
let forme_en_cours = ref forme_vide in
let moves = ref [] in
let scores_0 = Array.make 1000 0 in
let bit = [|1;1;1|] in
let bit_prec = [|1;1;1|] in
let bit_0 = ref 1 in
let historique = ref [] in

let cartesien (n,m) =
    let res = Array.make ((m-n+1)*(m-n+1)) (-1,-1) in
    let k = ref 0 in
    for i = n to m do
        for j = n to m do
            res.(!k) <- (i,j); incr k
        done;
    done;
    res
    in

let copy_matrix m =
    let l = Array.length m in
    if l = 0 then m
    else let result = Array.make l m.(0) in
    for i=0 to l-1 do
        result.(i) <- Array.copy m.(i)
    done;
    result
    in
    
let dedans (a,b) = not ( a > 34 || a < 0 || b > 19 || b < 0 ) in

let voisins (i,j) = 
        let res = ref [] in
        if dedans (i+1,j)
        then res := (i+1,j)::!res;
        if dedans (i-1,j)
        then res := (i-1,j)::!res;
        if dedans (i,j+1)
        then res := (i,j+1)::!res;
        if dedans (i,j-1)
        then res := (i,j-1)::!res;
        !res
        in


(* game loop *)
while true do
    let gameround = int_of_string (input_line stdin) in
    (* x: Your x position *)
    (* y: Your y position *)
    (* backintimeleft: Remaining back in time *)

    let line = input_line stdin in
    let x, y, backintimeleft = Scanf.sscanf line "%d %d %d" (fun x y backintimeleft -> (x, y, backintimeleft)) in
    for i = 0 to opponentcount - 1 do

        let line = input_line stdin in
        let opponentx, opponenty, opponentbackintimeleft = Scanf.sscanf line "%d %d %d" (fun opponentx opponenty opponentbackintimeleft -> (opponentx, opponenty, opponentbackintimeleft)) in
        pos.(i+1) <- (opponentx,opponenty);
        bit.(i) <- opponentbackintimeleft;
    done;

    let mat = Array.make_matrix 20 35 0 in 
    for i = 0 to 19 do
        let line = input_line stdin in
        for j = 0 to (String.length line) - 1 do
          let v = String.sub line j 1 in
          let v = match v with
                    | "." -> -1
                    | _ -> int_of_string v      
          in mat.(i).(j) <- v;
        done;
    done;
    
    let distance (a,b) (i,j) = abs (a - i) + abs (b - j) in
    
    let update_scores () =
        let res = ref 0 in
        for i = 0 to 34 do
            for j = 0 to 19 do
                if mat.(j).(i) = 0
                then incr res;
            done;
        done;
        scores_0.(gameround) <- !res;
        in

    let nb_libres () =
        let res = ref 0 in
        for i = 0 to 34 do
            for j = 0 to 19 do
                if mat.(j).(i) = -1
                then incr res;
            done;
        done;
        !res
        in
        
    
    let list_forme () =
        let l = nb_libres () in
        let taille = (match opponentcount with
                        | 1 ->  if l > 650 then (10,17)
                                else    if l > 450 then (4,12)
                                        else    if l > 100 then (2,10)
                                                else (1,9)
                        | 2 ->  if l > 300 then (5,12)
                                else    if l > 200 then (3,12)
                                        else    if l > 100 then (2,9)
                                                else (1,9)
                        | _ ->  if l > 300 then (5,12)
                                else    if l > 200 then (3,10)
                                        else    if l > 100 then (2,9)
                                                else (1,9)) in                                         
        let formats = cartesien taille in   
        let pf = Array.length formats in
        let res = ref [] in
        for k = 0 to pf-1 do
            let n,m = formats.(k) in
            for i = 0 to 35 - n do
                for j = 0 to 20 - m do
                    let compt = ref 0 in
                    let test = ref true in
                    for di = 0 to n - 1 do
                        for dj = 0 to m - 1 do
                            let mpsi = mat.(j + dj).(i + di) in
                            if mpsi > -1
                            then test := false;
                            if mpsi = 0
                            then incr compt;
                            if mpsi > 0
                            then compt := 42*42;
                        done;
                    done;
                    if !test || (!compt = 1 && (n,m) > (1,1))
                    then res := { coord = (i,j) ; format = (n,m) ; indice = 0 }::!res;                        
                done;
            done;
        done;
        !res
        in
    
    let chemin f =
        let i,j = f.coord in
        let n,m = f.format in
        let chem = ref [] in
        for k = 1 to n-1 do
            chem := (i+k,j)::!chem;
        done;
        for k = 1 to m-1 do
            chem := (i+n-1,j+k)::!chem;
        done;
        for k = n-2 downto 0 do
            chem := (i+k,j+m-1)::!chem;
        done;
        for k = m-2 downto 0 do
            chem := (i,j+k)::!chem;
        done;
                   
        let mini = ref max_int in
        let pres = ref (-1) in
        for k = 0 to (List.length !chem) - 1 do
            let dist = distance (x,y) (List.nth !chem k) in
            if dist < !mini
            then (  mini := dist;
                    pres := k);
        done;
        for i = 0 to !pres - 1 do
            let h::t = !chem in
                chem := t @ [h];
        done;
        if !chem = [] then chem := pos.(1)::[];
        let demi = -2 + (List.length !chem) / 2 in
        if demi > 0 then (
        let temp1 = ref 0 in
        let temp2 = ref 0 in
        for k = 0 to demi do
            let proches = voisins (List.nth !chem k) in
            for i = 0 to List.length proches -1 do
                let a,b = List.nth proches i in
                if mat.(b).(a) = 0 then incr temp1;
            done;
            
            let proches1 = voisins (List.nth !chem ( -k - 1 + List.length !chem)) in
            for i = 0 to List.length proches1 - 1 do
                let a,b = List.nth proches1 i in
                if mat.(b).(a) = 0 then incr temp2;
            done;
        done;
        if temp2 < temp1 then chem := List.rev !chem;);
        
        match n,m with
            | 1,1 -> [(i,j)]
            | 1,m -> [(i,j);(i,j+m-1)]
            | n,1 -> [(i,j);(i+n-1,j)]
            | n,2 -> [(i,j);(i+n-1,j+1)]
            | 2,m -> [(i,j);(i+1,j+m-1)]
            | _ -> !chem
        in
        
    let indices r =
        let p = Array.length r in
        for k = 0 to p-1 do
            let f = r.(k) in
            let res = ref (1000) in
            let i,j = f.coord in 
            let n,m = f.format in
            let dist_0 = (distance (i+n-1,j) (x,y)) + (distance (i+n-1,j+n-1) (x,y)) + (distance (i,j) (x,y)) + (distance (i,j+n-1) (x,y)) in
            let dist_opp = ref max_int in
            for k = 1 to opponentcount do
                dist_opp := !dist_opp + (distance (i+n-1,j) pos.(k)) + (distance (i+n-1,j+n-1) pos.(k)) + (distance (i,j) pos.(k)) + (distance (i,j+n-1) pos.(k));
            done;
        
            let float_dist_opp = float_of_int (2 * !dist_opp) in
            res := !res * (int_of_float (sqrt float_dist_opp)) / (dist_0*2 + 1);
            let aire = n*m in
                if gameround < 30 && opponentcount <3
            then res := !res * aire * aire * aire * !dist_opp
            else (  if opponentcount = 3
                    then res := !res * aire * aire * !dist_opp / (dist_0*3 + 1)
                    else res := !res * aire * aire * !dist_opp / (dist_0 + 1)
                 );
            r.(k).indice <- !res
        done;
        in
        
    let choix r =
        let maxi = ref min_int in
        let res = ref forme_vide in
        let p = Array.length r in
        for k = 0 to p-1 do
            if r.(k).indice > !maxi
            then (  maxi := r.(k).indice;
                    res := r.(k);
                 );
        done;
        !res
        in
               
    let remplir () =
        let rectangles = Array.of_list (list_forme ()) in
        indices rectangles;
        forme_en_cours := choix rectangles;
        moves := chemin !forme_en_cours;
        in
    
    let chek () =
        if !moves = [] || !moves = (x,y)::[] || !moves = (x,y)::(x,y)::[] then remplir ();
        if List.hd !moves = (x,y) then moves := List.tl !moves;
        in
    
    let probleme () =
        let res = ref false in
        let f = !forme_en_cours in
        let (i,j) = f.coord in
        let (n,m) = f.format in
        
        for di = 0 to n-1 do
            for dj = 0 to m-1 do
                if mat.(j+dj).(i+di) > 0
                then res := true
            done;
        done;
        
        update_scores ();
        if scores_0.(gameround) - scores_0.(gameround -1) > 1
        then res := true;
        
        if (bit <> bit_prec) then res := true;
        bit_prec.(0) <- bit.(0);
        bit_prec.(1) <- bit.(1);
        bit_prec.(2) <- bit.(2);
        
        !res
        in
        
    if probleme () then moves := [];   
    chek ();
    
    let ope_sabotage () =
        let voisins = [|(x+1,y);(x+2,y);(x+3,y);(x-1,y);(x-2,y);(x-3,y);(x,y+1);(x,y+2);(x,y+3);(x,y-1);(x,y-2);(x,y-3)|] in
        for k = 0 to 3 do
            let suspect = Array.sub voisins (3*k) 3 in
            let i,j = suspect.(0) in
            let a,b = suspect.(1) in
            let c,d = suspect.(2) in
            if dedans (i,j) && dedans (a,b)
            then (  if mat.(j).(i) > 0 && mat.(b).(a) = -1 
                    then moves := (a,b)::!moves;
                    compteur_sabotage := 20;
                 );
            if dedans (a,b) && dedans (c,d) && !compteur_sabotage <> 5
            then (  if mat.(b).(a) > 0 && mat.(d).(c) = -1 
                    then moves := (c,d)::!moves;
                    compteur_sabotage := 20;
                 );
            done;
        in
       
    if !compteur_sabotage <= 0 
    then ope_sabotage ();
        
    compteur_sabotage := !compteur_sabotage -1;

    
    prerr_endline ("Carré en cours" ^ " (" ^(string_of_int (fst !forme_en_cours.coord)) ^ "," ^ (string_of_int (snd !forme_en_cours.coord)) ^ ") " ^(string_of_int ( fst !forme_en_cours.format)) ^ "x" ^(string_of_int ( snd !forme_en_cours.format)) );
    prerr_endline ("Compteur pour le prochain carré : " ^ (string_of_int (List.length !moves))^ ".") ;
    let (x1,y1)::t = !moves in
    print_endline ( (string_of_int x1) ^ " " ^ (string_of_int y1));
    
done;
