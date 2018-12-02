module Y2015.Day22 (answer1, answer2) where

import Control.Monad.State
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)

answer1 :: IO ()
answer1 =
    let err = error "no way to win???"
    in  print $ fromMaybe err (miniMana (allBattles carryBattle initialState))

answer2 :: IO ()
answer2 =
    let err = error "no way to win???"
    in  print $ fromMaybe err (miniMana (allBattles carryHardBattle initialState))

data Spell = Missile | Drain | Shield | Poison | Recharge deriving (Show, Eq)
data BattleStatus = Ongoing | Lost | Won deriving (Show, Eq)
data BattleState = BattleState {
  bossHp :: Int,
  bossAtk :: Int,
  playerHp :: Int,
  mana :: Int,
  poisonTimer :: Int,
  shieldTimer :: Int,
  rechargeTimer :: Int,
  manaSpent :: Int
  } deriving (Show, Eq)

type Battle = State BattleState BattleStatus

data Rose a = Leaf | Rose a [Rose a] deriving (Show, Eq)

-- build a rose tree of all possible battles from a given transformation and an initial state
allBattles
    :: (BattleState -> Spell -> BattleState) -> BattleState -> Rose BattleState
allBattles f s = case battleStatus s of
    Won  -> Rose s []
    Lost -> Rose s []
    Ongoing ->
        let nextStates = map (f s) (castableSpells s)
        in  Rose s (map (allBattles f) nextStates)

-- It's surely possible to do the same thing with a Foldable instance
miniMana :: Rose BattleState -> Maybe Int
miniMana Leaf          = Nothing
miniMana (Rose s next) = case battleStatus s of
    Won     -> Just $ manaSpent s
    Lost    -> Nothing
    Ongoing -> minimumMay (mapMaybe miniMana next)

minimumMay [] = Nothing
minimumMay xs = Just $ minimum xs


-- a bit clunky
castableSpells :: BattleState -> [Spell]
castableSpells s =
    let
        m       = mana s
        missile = if m >= 53 then Just Missile else Nothing
        drain   = if m >= 73 then Just Drain else Nothing
        shield =
            if m >= 113 && shieldTimer s <= 1 then Just Shield else Nothing
        poison =
            if m >= 173 && poisonTimer s <= 1 then Just Poison else Nothing
        recharge =
            if m >= 229 && rechargeTimer s <= 1 then Just Recharge else Nothing
    in
        catMaybes [missile, drain, shield, poison, recharge]

initialState = BattleState
    { bossHp        = 71
    , bossAtk       = 10
    , playerHp      = 50
    , mana          = 500
    , poisonTimer   = 0
    , shieldTimer   = 0
    , rechargeTimer = 0
    , manaSpent     = 0
    }

carryBattle :: BattleState -> Spell -> BattleState
carryBattle s spell = bossAttack . applyEffect . cast spell . applyEffect $ s

carryHardBattle :: BattleState -> Spell -> BattleState
carryHardBattle s spell =
    bossAttack . applyEffect . cast spell . hardModeEffect . applyEffect $ s

battleStatus :: BattleState -> BattleStatus
battleStatus s | bossHp s <= 0   = Won
               | playerHp s <= 0 = Lost
               | otherwise       = Ongoing

cast :: Spell -> BattleState -> BattleState
cast Missile s = s { bossHp    = bossHp s - 4
                   , mana      = mana s - 53
                   , manaSpent = manaSpent s + 53
                   }
cast Drain s = s { bossHp    = bossHp s - 2
                 , playerHp  = playerHp s + 2
                 , mana      = mana s - 73
                 , manaSpent = manaSpent s + 73
                 }
cast Shield s = s { shieldTimer = 6
                  , mana        = mana s - 113
                  , manaSpent   = manaSpent s + 113
                  }
cast Poison s = s { poisonTimer = 6
                  , mana        = mana s - 173
                  , manaSpent   = manaSpent s + 173
                  }
cast Recharge s = s { rechargeTimer = 5
                    , mana          = mana s - 229
                    , manaSpent     = manaSpent s + 229
                    }

bossAttack :: BattleState -> BattleState
bossAttack s =
    let armor = if shieldTimer s > 0 then 7 else 0
        dmg   = bossAtk s - armor
    in  s { playerHp = playerHp s - dmg }

applyEffect :: BattleState -> BattleState
applyEffect = poisonTick . shieldTick . rechargeTick

rechargeTick :: BattleState -> BattleState
rechargeTick s = if rechargeTimer s > 0
    then s { rechargeTimer = rechargeTimer s - 1, mana = mana s + 101 }
    else s

poisonTick :: BattleState -> BattleState
poisonTick s = if poisonTimer s > 0
    then s { poisonTimer = poisonTimer s - 1, bossHp = bossHp s - 3 }
    else s

shieldTick s = s { shieldTimer = shieldTimer s - 1 }

hardModeEffect :: BattleState -> BattleState
hardModeEffect s =
    let newHp = playerHp s - 1
    in  if newHp <= 0
    -- cheating here to make sure the boss will win at this turn
            then s { bossHp = bossHp s + 100, playerHp = newHp }
            else s { playerHp = newHp }
