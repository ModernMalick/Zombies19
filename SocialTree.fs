namespace Zombies19

type SocialTree =
    | PersonNode of Person * SocialTree list
    | Empty