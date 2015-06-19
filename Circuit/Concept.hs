module Circuit.Concept (
    CircuitConcept (..),
    consistency, causality, (~>), orCausality, silent,
    buffer, inverter, cElement, meElement, andGate, orGate,
    me, handshake, handshake00, handshake11
    ) where

import Circuit

type CircuitConcept a = Concept (State a) (Transition a)

-- Event-based concepts
consistency :: CircuitConcept a
consistency = excitedConcept before

causality :: Eq a => Transition a -> Transition a -> CircuitConcept a
causality cause effect =
    excitedConcept $ \t -> if t /= effect
                           then const True
                           else after cause .&&. before effect

(~>) :: Eq a => Transition a -> Transition a -> CircuitConcept a
(~>) = causality

orCausality :: Eq a => Transition a -> Transition a -> Transition a -> CircuitConcept a
orCausality cause1 cause2 effect =
    excitedConcept $ \t -> if t /= effect
                           then const True
                           else (after cause1 .||. after cause2) .&&. before effect

silent :: Eq a => Transition a -> CircuitConcept a
silent t = excitedConcept $ const . (/= t)

-- Gate-level concepts
buffer :: Eq a => a -> a -> CircuitConcept a
buffer a b = rise a ~> rise b <> fall a ~> fall b

inverter :: Eq a => a -> a -> CircuitConcept a
inverter a b = rise a ~> fall b <> fall a ~> rise b

cElement :: Eq a => a -> a -> a -> CircuitConcept a
cElement a b c = buffer a c <> buffer b c

meElement :: Eq a => a -> a -> a -> a -> CircuitConcept a
meElement r1 r2 g1 g2 = buffer r1 g1 <> buffer r2 g2 <> me g1 g2

andGate :: Eq a => a -> a -> a -> CircuitConcept a
andGate a b c = rise a ~> rise c <> rise b ~> rise c <> orCausality (fall a) (fall b) (fall c)

orGate :: Eq a => a -> a -> a -> CircuitConcept a
orGate a b c = orCausality (rise a) (rise b) (rise c) <> fall a ~> fall c <> fall b ~> fall c

-- Protocol-level concepts
handshake :: Eq a => a -> a -> CircuitConcept a
handshake a b = buffer a b <> inverter b a

handshake00 :: Eq a => a -> a -> CircuitConcept a
handshake00 a b = handshake a b <> initialConcept both00
  where
    both00 = before (rise a) .&&. before (rise b)

handshake11 :: Eq a => a -> a -> CircuitConcept a
handshake11 a b = handshake a b <> initialConcept both11
  where
    both11 = before (fall a) .&&. before (fall b)

me :: Eq a => a -> a -> CircuitConcept a
me a b = fall a ~> rise b <> fall b ~> rise a <> initialConcept notBoth11 <> invariantConcept notBoth11
  where
    notBoth11 = before (rise a) .||. before (rise b)
