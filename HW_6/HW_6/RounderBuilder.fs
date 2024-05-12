module Rounder

open System

type RounderBuilder(accuracy : int) =
    member this.Bind(x : float, func) =
        func x

    member this.Return(x : float) = Math.Round(x, accuracy)