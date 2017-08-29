{-# LANGUAGE OverloadedStrings #-}
module Plugins.GreatestIdea.Types where

import qualified Data.Map.Strict  as Map
import qualified Data.Text        as T

data GestiGame = GestiGame {host :: T.Text, players :: [T.Text], assigned :: Map.Map T.Text (Role,Role,Role), choices :: Map.Map T.Text (Maybe Choice)} deriving (Show)

data Choice = Choice {choice :: Role, discard :: Role} deriving (Show)

data Role = Role {alignment :: Alignment, roleType :: RoleType} deriving (Show)

data Alignment = Town | Mafia | Werewolf | Survivor | Cult | CultOSRecruiter | Alien | Lyncher | SerialKiller | Judas | Saulus | OSTownie | Underdog | WildCardAlignment

instance Show Alignment where
  show Town = "Town"
  show Mafia = "Mafia"
  show Werewolf = "Werewolf"
  show Survivor = "Survivor"
  show Cult = "Cult"
  show CultOSRecruiter = "Cult OS Recruiter"
  show Alien = "Alien"
  show Lyncher = "Lyncher"
  show SerialKiller = "Serial Killer"
  show Judas = "Judas"
  show Saulus = "Saulus"
  show OSTownie = "OS Townie"
  show Underdog = "Underdog"
  show WildCardAlignment = "Wild Card"

data RoleType = Vanilla | Watcher | Tracker | Cop | CopLover | Seer | FBIAgent | Doctor | Roleblocker | Jailkeeper | Bodyguard | Vigilante | OSVigilante | OSDayvig | CompulsiveChildkiller | Bulletproof | Supersaint | OSPGO | Mason | MasonDoctor | MasonLover | Lover | JOAT | Vengeful | RetiredWerewolfHunter | RetiredMarine | Miller | Hirsute | Evangelistic | Tentacled | Watchlisted | UniMiller | BlackGoo | Ascetic | PrivateInvestigator | Gravedigger | Nymphomaniac | OSGovernor | OSUL | Godfather | InnocentChild | Hider | Enabler | Treestump | ConspiracyTheorist | OSKingmaker | WeakJailkeeper | Bloodhound | VanillaCop | Hero | Tourist | Nurse | OSCommuter | COAT | OSGladiator | Lynchbait | Psychiatrist | Reloader | FruitVendor | Parrot | Strongman | ReflexiveDoctor | Cupid | Alpha | CompulsiveHider | OSBulletproof | Ninja | Prober | Vanillaiser | Silencer | BulletproofLover | Psychotrooper | MassRedirector | Bloodsucker | Sympathiser | CompulsiveBodyguard | TSBulletproof | MafiaKillImmune | WerewolfKillImmune | AlienKillImmune | OSGoomaker | WildCardRole

instance Show RoleType where
  show Vanilla = "(vanilla)"
  show Watcher = "Watcher"
  show Tracker = "Tracker"
  show Cop = "Cop"
  show CopLover = "Cop Lover"
  show Seer = "Seer"
  show FBIAgent = "FBI Agent"
  show Doctor = "Doctor"
  show Roleblocker = "Roleblocker"
  show Jailkeeper = "Jailkeeper"
  show Bodyguard = "Bodyguard"
  show Vigilante = "Vigilante"
  show OSVigilante = "One-shot Vigilante"
  show OSDayvig = "One-shot Dayvig"
  show CompulsiveChildkiller = "Compulsive Childkiller (If any Innocent Child is revealed, the Childkiller must immediately dayvig that player)"
  show Bulletproof = "(bulletproof)"
  show Supersaint = "Supersaint"
  show OSPGO = "One-shot Paranoid Gun Owner"
  show Mason = "Mason"
  show MasonDoctor = "Mason Doctor"
  show MasonLover = "Mason Lover"
  show Lover = "Lover"
  show JOAT = "Jack-of-all-trades (One Roleblock, One Cop, One Doctor)"
  show Vengeful = "(vengeful)"
  show RetiredWerewolfHunter = "Retired Werewolf Hunter (named townie/goon)"
  show RetiredMarine = "Retired Marine (Immune to Serial Killer kills)"
  show Miller = "Miller"
  show Hirsute = "Hirsute (Investigates as Werewolf)"
  show Evangelistic = "Evangelistic (Investigates as Cult)"
  show Tentacled = "Tentacled (Investigates as Alien)"
  show Watchlisted = "Watchlisted (Investigates as Serial Killer)"
  show UniMiller = "Universal Miller"
  show BlackGoo = "Black Goo (Anyone who targets it with an action becomes Cult)"
  show Ascetic = "(ascetic)"
  show PrivateInvestigator = "Private Investigator (Gets result \"Cult\" or \"Not Cult\")"
  show Gravedigger = "Gravedigger (Shows up as targeting all nightkilled players to Trackers and Watchers on night of said players' deaths)"
  show Nymphomaniac = "Nymphomaniac (Compulsively chooses a Lover on Night 1; NOT part of pre-existing Lover groups unless united by N1 choice)"
  show OSGovernor = "One-shot Governor"
  show OSUL = "(One-shot Unlynchable)"
  show Godfather = "Godfather"
  show InnocentChild = "Innocent Child"
  show Hider = "Hider"
  show Enabler = "Enabler"
  show Treestump = "Treestump"
  show ConspiracyTheorist = "Conspiracy Theorist (Gets result \"Alien\" or \"Not Alien\"; investigates as \"Alien\")"
  show OSKingmaker = "One-shot Kingmaker"
  show WeakJailkeeper = "Weak Jailkeeper"
  show Bloodhound = "Bloodhound (Gets result \"Town\" or \"Not Town\")"
  show VanillaCop = "Vanilla Cop (VTs, basic Werewolves and Mafia Goons are Vanilla)"
  show Hero = "Hero (If a King tries to execute you, the King dies instead)"
  show Tourist = "Tourist (Compulsively targets someone every night. No effect)"
  show Nurse = "Nurse (If a Town Doctor dies, you inherit their power)"
  show OSCommuter = "One-shot Commuter"
  show COAT = "Cop-of-all-Trades (One-shot Cop, One-shot Seer, One-shot FBI Agent, One-shot Conspiracy Theorist, One-shot Private Investigator)"
  show OSGladiator = "One-shot Gladiator (Target two players at night; if both alive at daybreak, they are the only two lynch candidates that day)"
  show Lynchbait = "Lynchbait (If you are lynched, any and all Lynchers immediately win)"
  show Psychiatrist = "Psychiatrist (Target someone each night, if they are an SK they will become a Vanilla Townie)"
  show Reloader = "Reloader (Target someone each night, if they have previously used a one-shot ability they regain their shot)"
  show FruitVendor = "Fruit Vendor (Target someone each night. They will be told that they received fruit)"
  show Parrot = "Parrot (Target someone each night. If they have an active ability, you use that ability on them)"
  show Strongman = "Strongman"
  show ReflexiveDoctor = "Reflexive Doctor (protects anyone who targets them)"
  show Cupid = "Cupid (Targets player Night 1; all OTHER players who targeted same player become lovers with target)"
  show Alpha = "(alpha)"
  show CompulsiveHider = "Compulsive Hider"
  show OSBulletproof = "One-shot Bulletproof"
  show Ninja = "(ninja)"
  show Prober = "Prober (targets one player; target is roleblocked and Prober gets investigation result of \"Werewolf\" or \"Not werewolf\")"
  show Vanillaiser = "Vanillaiser"
  show Silencer = "Silencer"
  show BulletproofLover = "(bulletproof) Lover"
  show Psychotrooper = "Psychotrooper (While alive, all cops with 'guilty/not guilty' format results are Insane)"
  show MassRedirector = "Mass Redirector (Once per game, choose a player at night. All actions are redirected to that player that night, making the Mass Redirector essentially a One-Shot Lightning Rod-izer)"
  show Bloodsucker = "Bloodsucker (Treestump someone at night. No Scum partner may perform the factional nightkill on the same night that you do this)"
  show Sympathiser = "Sympathiser (If there are other scum of your faction, you are a Goon of that faction. Otherwise, you are a VT.)"
  show CompulsiveBodyguard = "(compulsive bodyguard)"
  show TSBulletproof = "(Two-shot Bulletproof)"
  show MafiaKillImmune = "(Immune to Mafia kills)"
  show WerewolfKillImmune = "(Immune to Werewolf kills)"
  show AlienKillImmune = "(Immune to Alien & Replicant kills)"
  show OSGoomaker = "One-shot Goomaker (Once per game, at night, target someone to make them take on Black Goo ability)"
  show WildCardRole = "Wild Card (If picked as alignment, after discards are revealed, a random alignment is drawn from the remaining cards. If picked as role, after discards are revealed, a random role is drawn from the remaining cards.)"

roleList :: [Role]
roleList = [
  Role {alignment=Town, roleType=Vanilla},
  Role {alignment=Town, roleType=Vanilla},
  Role {alignment=Town, roleType=Vanilla},
  Role {alignment=Town, roleType=Vanilla},
  Role {alignment=Town, roleType=Vanilla},
  Role {alignment=Town, roleType=Vanilla},
  Role {alignment=Town, roleType=Vanilla},
  Role {alignment=Town, roleType=Vanilla},
  Role {alignment=Town, roleType=Vanilla},
  Role {alignment=Town, roleType=Vanilla},
  Role {alignment=Town, roleType=Vanilla},
  Role {alignment=Town, roleType=Vanilla},
  Role {alignment=Town, roleType=Watcher},
  Role {alignment=Town, roleType=Tracker},
  Role {alignment=Town, roleType=Tracker},
  Role {alignment=Town, roleType=Cop},
  Role {alignment=Town, roleType=Cop},
  Role {alignment=Town, roleType=CopLover},
  Role {alignment=Town, roleType=Seer},
  Role {alignment=Town, roleType=Seer},
  Role {alignment=Town, roleType=FBIAgent},
  Role {alignment=Town, roleType=Doctor},
  Role {alignment=Town, roleType=Doctor},
  Role {alignment=Town, roleType=Roleblocker},
  Role {alignment=Town, roleType=Jailkeeper},
  Role {alignment=Town, roleType=Bodyguard},
  Role {alignment=Town, roleType=Vigilante},
  Role {alignment=Town, roleType=OSVigilante},
  Role {alignment=Town, roleType=OSDayvig},
  Role {alignment=Town, roleType=CompulsiveChildkiller},
  Role {alignment=Town, roleType=Bulletproof},
  Role {alignment=Town, roleType=Supersaint},
  Role {alignment=Town, roleType=OSPGO},
  Role {alignment=Town, roleType=Mason},
  Role {alignment=Town, roleType=Mason},
  Role {alignment=Town, roleType=Mason},
  Role {alignment=Town, roleType=Mason},
  Role {alignment=Town, roleType=MasonDoctor},
  Role {alignment=Town, roleType=MasonLover},
  Role {alignment=Town, roleType=Lover},
  Role {alignment=Town, roleType=Lover},
  Role {alignment=Town, roleType=Lover},
  Role {alignment=Town, roleType=JOAT},
  Role {alignment=Town, roleType=Vengeful},
  Role {alignment=Town, roleType=RetiredWerewolfHunter},
  Role {alignment=Town, roleType=RetiredMarine},
  Role {alignment=Town, roleType=Miller},
  Role {alignment=Town, roleType=Hirsute},
  Role {alignment=Town, roleType=Evangelistic},
  Role {alignment=Town, roleType=Tentacled},
  Role {alignment=Town, roleType=Watchlisted},
  Role {alignment=Town, roleType=UniMiller},
  Role {alignment=Town, roleType=BlackGoo},
  Role {alignment=Town, roleType=Ascetic},
  Role {alignment=Town, roleType=PrivateInvestigator},
  Role {alignment=Town, roleType=Gravedigger},
  Role {alignment=Town, roleType=Nymphomaniac},
  Role {alignment=Town, roleType=OSGovernor},
  Role {alignment=Town, roleType=OSUL},
  Role {alignment=Town, roleType=Godfather},
  Role {alignment=Town, roleType=InnocentChild},
  Role {alignment=Town, roleType=Hider},
  Role {alignment=Town, roleType=Enabler},
  Role {alignment=Town, roleType=Treestump},
  Role {alignment=Town, roleType=ConspiracyTheorist},
  Role {alignment=Town, roleType=ConspiracyTheorist},
  Role {alignment=Town, roleType=ConspiracyTheorist},
  Role {alignment=Town, roleType=OSKingmaker},
  Role {alignment=Town, roleType=WeakJailkeeper},
  Role {alignment=Town, roleType=Bloodhound},
  Role {alignment=Town, roleType=VanillaCop},
  Role {alignment=Town, roleType=Hero},
  Role {alignment=Town, roleType=Tourist},
  Role {alignment=Town, roleType=Nurse},
  Role {alignment=Town, roleType=OSCommuter},
  Role {alignment=Town, roleType=COAT},
  Role {alignment=Town, roleType=OSGladiator},
  Role {alignment=Town, roleType=Lynchbait},
  Role {alignment=Town, roleType=Psychiatrist},
  Role {alignment=Town, roleType=Reloader},
  Role {alignment=Town, roleType=FruitVendor},
  Role {alignment=Town, roleType=Parrot},
  Role {alignment=Judas, roleType=Vanilla},
  Role {alignment=Saulus, roleType=Vanilla},
  Role {alignment=OSTownie, roleType=Vanilla},
  Role {alignment=Underdog, roleType=Vanilla},
  Role {alignment=Mafia, roleType=Vanilla},
  Role {alignment=Mafia, roleType=Vanilla},
  Role {alignment=Mafia, roleType=Vanilla},
  Role {alignment=Mafia, roleType=Vanilla},
  Role {alignment=Mafia, roleType=Vanilla},
  Role {alignment=Mafia, roleType=Godfather},
  Role {alignment=Mafia, roleType=Tracker},
  Role {alignment=Mafia, roleType=Doctor},
  Role {alignment=Mafia, roleType=Roleblocker},
  Role {alignment=Mafia, roleType=Lover},
  Role {alignment=Mafia, roleType=Seer},
  Role {alignment=Mafia, roleType=OSDayvig},
  Role {alignment=Mafia, roleType=OSGovernor},
  Role {alignment=Mafia, roleType=Strongman},
  Role {alignment=Mafia, roleType=ReflexiveDoctor},
  Role {alignment=Mafia, roleType=Hirsute},
  Role {alignment=Mafia, roleType=Cupid},
  Role {alignment=Mafia, roleType=Alpha},
  Role {alignment=Mafia, roleType=CompulsiveHider},
  Role {alignment=Mafia, roleType=FruitVendor},
  Role {alignment=Werewolf, roleType=Vanilla},
  Role {alignment=Werewolf, roleType=Vanilla},
  Role {alignment=Werewolf, roleType=Vanilla},
  Role {alignment=Werewolf, roleType=Vanilla},
  Role {alignment=Werewolf, roleType=Alpha},
  Role {alignment=Werewolf, roleType=Roleblocker},
  Role {alignment=Werewolf, roleType=OSBulletproof},
  Role {alignment=Werewolf, roleType=Cop},
  Role {alignment=Werewolf, roleType=Mason},
  Role {alignment=Werewolf, roleType=Watcher},
  Role {alignment=Werewolf, roleType=FBIAgent},
  Role {alignment=Werewolf, roleType=Ninja},
  Role {alignment=Werewolf, roleType=OSPGO},
  Role {alignment=Werewolf, roleType=Miller},
  Role {alignment=Werewolf, roleType=Supersaint},
  Role {alignment=Werewolf, roleType=Godfather},
  Role {alignment=Werewolf, roleType=Gravedigger},
  Role {alignment=Werewolf, roleType=Parrot},
  Role {alignment=Alien, roleType=OSUL},
  Role {alignment=Alien, roleType=Prober},
  Role {alignment=Alien, roleType=Vanillaiser},
  Role {alignment=Alien, roleType=Silencer},
  Role {alignment=Alien, roleType=BulletproofLover},
  Role {alignment=Alien, roleType=Psychotrooper},
  Role {alignment=Alien, roleType=MassRedirector},
  Role {alignment=Alien, roleType=Bloodsucker},
  Role {alignment=Alien, roleType=Sympathiser},
  Role {alignment=Survivor, roleType=Vanilla},
  Role {alignment=Survivor, roleType=CompulsiveBodyguard},
  Role {alignment=Survivor, roleType=Mason},
  Role {alignment=Lyncher, roleType=Vanilla},
  Role {alignment=Lyncher, roleType=Vanilla},
  Role {alignment=SerialKiller, roleType=TSBulletproof},
  Role {alignment=SerialKiller, roleType=MafiaKillImmune},
  Role {alignment=SerialKiller, roleType=WerewolfKillImmune},
  Role {alignment=SerialKiller, roleType=AlienKillImmune},
  Role {alignment=CultOSRecruiter, roleType=Vanilla},
  Role {alignment=Cult, roleType=OSGoomaker},
  Role {alignment=WildCardAlignment, roleType=WildCardRole}]

prettyShowRole :: Role -> T.Text
prettyShowRole role = (T.pack . show $ alignment role) `T.append` " " `T.append` (T.pack . show $ roleType role)