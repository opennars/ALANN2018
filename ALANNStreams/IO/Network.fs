 (*
 * The MIT License
 *
 * Copyright 2018 The ALANN2018 authors.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *)

module Network

open System.Net
open System.Net.Sockets
open System.Text

let serverAddr = Params.SERVER_ADDR
let clientAddr = Params.CLIENT_ADDR
let serverPort = Params.SERVER_PORT
let clientPort = Params.GUI_CLIENT_PORT
let pongPort = Params.PONG_CLIENT_PORT
let shellAddr = Params.SHELL_ADDR
let shellPort = Params.SHELL_PORT

let clientEndPoint = new IPEndPoint(IPAddress.Parse(clientAddr), clientPort)
let shellEndPoint = new IPEndPoint(IPAddress.Parse(shellAddr), shellPort)
let outSocketUI = new UdpClient()
let outSocketShell = new UdpClient();

outSocketShell.Connect(shellEndPoint)

let sendMessageToClient (msg : string) =
    let data = Encoding.ASCII.GetBytes(msg)
    outSocketUI.SendAsync(data, data.Length) |> ignore
    outSocketShell.SendAsync(data, data.Length) |> ignore

let serverEndPoint = new IPEndPoint(IPAddress.Any, serverPort)
let inSocket = new UdpClient(serverEndPoint)

let getServerMsg(inSocket : UdpClient) =
    let asyncData = inSocket.Receive(ref serverEndPoint )
    Encoding.ASCII.GetString(asyncData)

let pongEndPoint = new IPEndPoint(IPAddress.Parse(clientAddr), pongPort)
let pongOutSocket = new UdpClient()

pongOutSocket.Connect(pongEndPoint)

let sendMessageToPong (msg : string) =
    let data = Encoding.ASCII.GetBytes(msg)
    pongOutSocket.SendAsync(data, data.Length) |> ignore