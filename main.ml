open Random

type joueur = {
    mutable hp : int;
    mutable hp_max : int;
    mutable score : int;
    mutable score_streak : float;
}

type tiles = {
    mutable typ : char;
    mutable position : int;
}

let map = [0,0,0,0,0]

let create_player hp = 
    {hp = hp; hp_max=hp; score = 0; score_streak = 0}

(*let read_next =*) 

    (*Ici faudra mettre toute la partie du code qui lit la prochaine ligne et 
    modifie la liste map *)

let lose =
    Printf.sprintf "Et c'est perdu !"

let win =
    Printf.sprintf "Et c'est gagné !"

let hit = 
    (*La fonction qui agit quand le joueur marque un point, oui c'était assez évident *)
    joueur.score <- joueur.score + 100;
    if joueur.hp < 500 then joueur.hp <- joueur.hp + 50 else joueur.hp <- joueur.hp + 3
    read_next;

let fail = 
    (*Soit le joueur a encore des hp et il en perd, soit il a perdu*)
    if joueur.hp>80 then joueur.hp <- joueur.hp-80 else lose;


(*let verif_tile map= 

(*Pas codable tant que j'ai pas tout le reste, mais bon 
en gros ça compare le nom de la tile en question à la touche et ça agit en conséquence*)

    let touche = wait_next_event [Key_pressed] in
    if tile.type = touche then hit else fail*)

(*let loop = *)

(*Le loop de base pas encore créé*)
