using System;
using Akka.Streams;
using Akka.Streams.Stage;
using C5;

namespace PriorityBuffer
{
    public class MyBuffer<T> : GraphStage<FlowShape<T, T>>
    {
        int _size;

        private sealed class Logic : GraphStageLogic
        {
            private readonly MyBuffer<T> _buffer;
            private readonly IPriorityQueue<T> _queue;
            private bool _downstreamWaiting = false;
            private int _size;
            private static Random rnd = new Random();

            public Logic(MyBuffer<T> buffer, int n) : base(buffer.Shape)
            {
                _buffer = buffer;
                _queue = new IntervalHeap<T>();
                _size = n;

                SetHandler(buffer.In, OnPush, OnUpstreamFinish);
                SetHandler(buffer.Out, OnPull);
            }

            private bool BufferFull => _queue.Count == _size;

            private T GetElement()
            {
                return _queue.DeleteMax();
            }

            private void OnPush()
            {
                var element = Grab(_buffer.In);
                _queue.Add(element);

                if (_queue.Count >= _size)
                {
                    _queue.DeleteMin();
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

        public MyBuffer(int n)
        {
            _size = n;

            Shape = new FlowShape<T, T>(In, Out);
        }

        public Inlet<T> In { get; } = new Inlet<T>("MyBuffer.in");

        public Outlet<T> Out { get; } = new Outlet<T>("MyBuffer.out");

        public override FlowShape<T, T> Shape { get; }

        protected override GraphStageLogic CreateLogic(Attributes inheritedAttributes) => new Logic(this, _size);
    }
}
