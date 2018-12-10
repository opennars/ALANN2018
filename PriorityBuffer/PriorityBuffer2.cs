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

using System;
using Akka.Streams;
using Akka.Streams.Stage;
using C5;

namespace PriorityBuffer2
{
    public class MyBuffer<T> : GraphStage<FlowShape<T, T>>
    {
        int _size;
        float _selectionFactor;

        private sealed class Logic : GraphStageLogic
        {
            private readonly MyBuffer<T> _buffer;
            private readonly IIndexedSorted<T> _queue;
            private bool _downstreamWaiting = false;
            private int _size;
            private float _selectionfactor;
            private static Random rnd = new Random();

            public Logic(MyBuffer<T> buffer, int n, float selectionFactor) : base(buffer.Shape)
            {
                _buffer = buffer;
                _queue = new TreeBag<T>();
                _size = n;
                _selectionfactor = selectionFactor;

                SetHandler(buffer.In, OnPush, OnUpstreamFinish);
                SetHandler(buffer.Out, OnPull);
            }

            private bool BufferFull => _queue.Count == _size;

            private T GetElement()
            {
                var f = Math.Pow(rnd.NextDouble(), _selectionfactor);
                var n = _queue.Count;
                var i = (int)(n * f);
                //Console.WriteLine("Pulling {0} of {1} [{2}]", i, n, f);
                var element = _queue[i];
                _queue.RemoveAt(i);
                return element;
            }

            private void OnPush()
            {
                var element = Grab(_buffer.In);
                _queue.Add(element);

                if (_queue.Count >= _size)
                {
                    var deleted = _queue.DeleteMin();
                    //Console.WriteLine("Deleted {0}", deleted);
                }

                if (_downstreamWaiting)
                {
                    _downstreamWaiting = false;
                    var bufferedElement = GetElement();
                    Push(_buffer.Out, bufferedElement);
                }

                if (!BufferFull)
                    Pull(_buffer.In);
            }

            private void OnUpstreamFinish()
            {
                if (_queue.Count != 0)
                {
                    EmitMultiple(_buffer.Out, _queue);
                }
                CompleteStage();
            }

            private void OnPull()
            {
                if (_queue.Count == 0)
                    _downstreamWaiting = true;
                else
                {
                    var element = GetElement();
                    Push(_buffer.Out, element);
                }

                if (!BufferFull && !HasBeenPulled(_buffer.In))
                    Pull(_buffer.In);
            }
        }

        public MyBuffer(int n, float selectionFactor)
        {
            _size = n;
            _selectionFactor = selectionFactor;

            Shape = new FlowShape<T, T>(In, Out);
        }

        public Inlet<T> In { get; } = new Inlet<T>("MyBuffer.in");

        public Outlet<T> Out { get; } = new Outlet<T>("MyBuffer.out");

        public override FlowShape<T, T> Shape { get; }

        protected override GraphStageLogic CreateLogic(Attributes inheritedAttributes) => new Logic(this, _size, _selectionFactor);
    }
}
