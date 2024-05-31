module Rounder

open System

type RounderBuilder(accuracy : int) =
    member this.Bind(x : float, func) =
        func (Math.Round(x, accuracy))

    member this.Return(x : float) = Math.Round(x, accuracy)