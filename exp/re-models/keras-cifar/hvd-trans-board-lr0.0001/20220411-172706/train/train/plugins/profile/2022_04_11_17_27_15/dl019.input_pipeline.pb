  *�C�l�ذ@��"�;a�@2�
VIterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2�|���8@!��\3�:@)|���8@1��\3�:@:Preprocessing2
GIterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch�g~5#@!�@���$@)g~5#@1�@���$@:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2::AssertCardinality::ParallelInterleaveV4��I��4�!@!�i��v#@)�I��4�!@1�i��v#@:Preprocessing2�
xIterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2�r���@!���� @)r���@1���� @:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2��UH�I�@! 4<V�J@)�UH�I�@1 4<V�J@:Preprocessing2�
_Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle��N�T*@!|��W5�,@)���k@1���ҽ�@:Preprocessing2�
iIterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch��l�=@!oygܬx@)�l�=@1oygܬx@:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl�S�A�т'@!��jo�)@)�B;�Y0@1���~<�@:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2::AssertCardinality��ݯ|g+@!Nɓ�!�-@)�'��
@1 ��I�@:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2::AssertCardinality::ParallelInterleaveV4[0]::FlatMap[0]::TFRecord��4(�@@!�����@)�4(�@@1�����@:Advanced file read2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2::AssertCardinality::ParallelInterleaveV4[0]::FlatMap�;�%8�	 @!2�r�t!@)HG�ŧ@1P���(#@:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCache�-B�4e.@!pz9�=�0@)g׽��@1�g�0�@:Preprocessing2]
&Iterator::Model::MaxIntraOpParallelismÜ�M�?!B��?)�`7l[��?1�̔�֛?:Preprocessing2g
0Iterator::Model::MaxIntraOpParallelism::Prefetcht%�?��?!u-S��X�?)t%�?��?1u-S��X�?:Preprocessing2t
=Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch�Q�|�?!��԰v�?)�Q�|�?1��԰v�?:Preprocessing2F
Iterator::Model�|A�?!
ò��A�?)F�T�=�o?17��?Oq?:Preprocessing:�
]Enqueuing data: you may want to combine small input data chunks into fewer but larger chunks.
�Data preprocessing: you may increase num_parallel_calls in <a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#map" target="_blank">Dataset map()</a> or preprocess the data OFFLINE.
�Reading data from files in advance: you may tune parameters in the following tf.data API (<a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#prefetch" target="_blank">prefetch size</a>, <a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#interleave" target="_blank">interleave cycle_length</a>, <a href="https://www.tensorflow.org/api_docs/python/tf/data/TFRecordDataset#class_tfrecorddataset" target="_blank">reader buffer_size</a>)
�Reading data from files on demand: you should read data IN ADVANCE using the following tf.data API (<a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#prefetch" target="_blank">prefetch</a>, <a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#interleave" target="_blank">interleave</a>, <a href="https://www.tensorflow.org/api_docs/python/tf/data/TFRecordDataset#class_tfrecorddataset" target="_blank">reader buffer</a>)
�Other data reading or processing: you may consider using the <a href="https://www.tensorflow.org/programmers_guide/datasets" target="_blank">tf.data API</a> (if you are not using it now)�
:type.googleapis.com/tensorflow.profiler.BottleneckAnalysisk
unknownTNo step time measured. Therefore we cannot tell where the performance bottleneck is.no*noZno#You may skip the rest of this page.BZ
@type.googleapis.com/tensorflow.profiler.GenericStepTimeBreakdown
  " * 2 : B J R Z b JGPUb��No step marker observed and hence the step time is unknown. This may happen if (1) training steps are not instrumented (e.g., if you are not using Keras) or (2) the profiling duration is shorter than the step time. For (1), you need to add step instrumentation; for (2), you may try to profile longer.