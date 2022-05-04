{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where 

import Data.Functor.Identity
import Data.Functor.Compose 
import Data.Functor.Const
import Data.Functor

data ConfigTemplate f = ConfigTemplate {
    _port :: f Int,
    _name :: f String,
    _file :: f FilePath
}

type Record t  = t Identity
type Partial t = t Maybe

type Config = Record ConfigTemplate       -- ConfigTemplate Identity 
type MaybeConfig = Partial ConfigTemplate -- ConfigTemplate Maybe


type f ~> g = forall x. f x -> g x

class FFunctor f where
    ffmap :: (Functor g, Functor h) => (g ~> h) -> f g -> f h

-- identity
-- ffmap id = id
-- composition
-- ffmap (f . g) = ffmap f . ffmap g

instance FFunctor ConfigTemplate where
    ffmap eta (ConfigTemplate port name file) = 
        ConfigTemplate (eta port) (eta name) (eta file)

generalize :: Applicative f => Identity a -> f a
generalize (Identity x) = pure x

toPartial :: FFunctor t => Record t -> Partial t
toPartial = ffmap generalize

class FFunctor t => FTraversable t where
    ftraverse :: (Functor f, Functor g, Applicative a)
                 => (f ~> Compose a g) -> t f -> a (t g)
    ftraverse eta = fsequence . ffmap eta

    fsequence :: (Functor f, Applicative a)
              => t (Compose a f) -> a (t f)
    fsequence = ftraverse id


ffmapDefault :: (Functor f, Functor g, FTraversable t)
             => (f ~> g) -> t f -> t g
ffmapDefault eta =
    runIdentity . ftraverse (Compose . Identity . eta)

fsequence' :: (FTraversable t, Applicative a) => t a -> a (Record t)
fsequence' = ftraverse (Compose . fmap Identity)

-- naturality
--nu . ftraverse eta = ftraverse (Compose . nu . getCompose . eta)
-- for any applicative transformation nu
-- identity
--ftraverse (Compose . Identity) = Identity
-- composition
--ftraverse (Compose . Compose . fmap (getCompose.phi) . getCompose . eta)
--    = Compose . fmap (ftraverse phi) . ftraverse eta


instance FTraversable ConfigTemplate where
    ftraverse eta (ConfigTemplate port name file)
        = ConfigTemplate <$>
            (getCompose $ eta port) <*>
            (getCompose $ eta name) <*>
            (getCompose $ eta file) 


toRecord :: FTraversable t => Partial t -> Maybe (Record t)
toRecord = ftraverse (Compose . fmap Identity)

toConfig :: MaybeConfig -> Maybe Config
toConfig = toRecord

data Empty a deriving Functor

ffoldMap :: forall f t m. (Monoid m, Functor f, FTraversable t)
         => (f () -> m) -> t f -> m
ffoldMap f = getConst . ftraverse mkConst
    where
        -- using ScopedTypeVariables to bind f
        mkConst :: f x -> Compose (Const m) Empty x
        mkConst = Compose . Const . f . ($> ())

config :: Applicative f => Int -> String -> FilePath -> ConfigTemplate f
config port name file = ConfigTemplate (pure port) (pure name) (pure file)



--example
--- create a MaybeConfig type with all Just
-- config 4 "ciao" "ei" :: MaybeConfig
--- I can create a Record
-- _name <$> toConfig mc