�%  *�$��1s@� �r1A2�
xIterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV20�B�_�\@!z�QV��S@)�B�_�\@1z�QV��S@:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV20ddY0�3@!:yEN5�+@)ddY0�3@1:yEN5�+@:Preprocessing2�
VIterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2��ZD�@!�@�$�
@)�ZD�@1�@�$�
@:Preprocessing2�
_Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle�ByGs� @!�<�X�g�?)\��AA��?1�ҫp.�?:Preprocessing2
GIterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch���]M��?!ҹ�R���?)��]M��?1ҹ�R���?:Preprocessing2�
iIterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch��U1�~�?! �����?)�U1�~�?1 �����?:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2::AssertCardinality::ParallelInterleaveV41��?�߾�?!^����?)��?�߾�?1^����?:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2::AssertCardinality::ParallelInterleaveV4[0]::FlatMap[0]::TFRecord1p�h����?!�����?)p�h����?1�����?:Advanced file read2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl01�JZ��3@!Ɇc��,@)8�9@0�?1d����?:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2::AssertCardinality1�-�熦�?!)-�)��?)��W\�?1�;d"`5�?:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCache0��ڊ��3@!�Dg#�=,@)/��0�?1��ށL߳?:Preprocessing2�
�Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch::Prefetch::ParallelMapV2::Shuffle::Prefetch::ParallelMapV2::MemoryCacheImpl::ParallelMapV2::AssertCardinality::ParallelInterleaveV4[0]::FlatMap1$���E��?!���m�K�?)�~����?1f��Tz�?:Preprocessing2]
&Iterator::Model::MaxIntraOpParallelism\kF��?!{�&��?)}�1Y��?1�lKj�?:Preprocessing2g
0Iterator::Model::MaxIntraOpParallelism::Prefetch;r�30�?!;��؊?);r�30�?1;��؊?:Preprocessing2t
=Iterator::Model::MaxIntraOpParallelism::Prefetch::MapAndBatch~b��U�?!? ���Ȅ?)~b��U�?1? ���Ȅ?:Preprocessing2F
Iterator::Model�����Y�?!�Ge�׫�?)c��Ju?1lx�
~�m?:Preprocessing:�
]Enqueuing data: you may want to combine small input data chunks into fewer but larger chunks.
�Data preprocessing: you may increase num_parallel_calls in <a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#map" target="_blank">Dataset map()</a> or preprocess the data OFFLINE.
�Reading data from files in advance: you may tune parameters in the following tf.data API (<a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#prefetch" target="_blank">prefetch size</a>, <a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#interleave" target="_blank">interleave cycle_length</a>, <a href="https://www.tensorflow.org/api_docs/python/tf/data/TFRecordDataset#class_tfrecorddataset" target="_blank">reader buffer_size</a>)
�Reading data from files on demand: you should read data IN ADVANCE using the following tf.data API (<a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#prefetch" target="_blank">prefetch</a>, <a href="https://www.tensorflow.org/api_docs/python/tf/data/Dataset#interleave" target="_blank">interleave</a>, <a href="https://www.tensorflow.org/api_docs/python/tf/data/TFRecordDataset#class_tfrecorddataset" target="_blank">reader buffer</a>)
�Other data reading or processing: you may consider using the <a href="https://www.tensorflow.org/programmers_guide/datasets" target="_blank">tf.data API</a> (if you are not using it now)�
:type.googleapis.com/tensorflow.profiler.BottleneckAnalysisk
unknownTNo step time measured. Therefore we cannot tell where the performance bottleneck is.no*noZno#You may skip the rest of this page.BZ
@type.googleapis.com/tensorflow.profiler.GenericStepTimeBreakdown
  " * 2 : B J R Z b JGPUb��No step marker observed and hence the step time is unknown. This may happen if (1) training steps are not instrumented (e.g., if you are not using Keras) or (2) the profiling duration is shorter than the step time. For (1), you need to add step instrumentation; for (2), you may try to profile longer.Y      Y@q�n�]<��?"�
unknownTNo step time measured. Therefore we cannot tell where the performance bottleneck is.b
`input_pipeline_analyzer (especially Section 3 for the breakdown of input operations on the Host)Q
Otf_data_bottleneck_analysis (find the bottleneck in the tf.data input pipeline)m
ktrace_viewer (look at the activities on the timeline of each Host Thread near the bottom of the trace view)"O
Mtensorflow_stats (identify the time-consuming operations executed on the GPU)"U
Strace_viewer (look at the activities on the timeline of each GPU in the trace view)*�
�<a href="https://www.tensorflow.org/guide/data_performance_analysis" target="_blank">Analyze tf.data performance with the TF Profiler</a>*y
w<a href="https://www.tensorflow.org/guide/data_performance" target="_blank">Better performance with the tf.data API</a>2M
=type.googleapis.com/tensorflow.profiler.GenericRecommendation
nono2no:
Refer to the TF2 Profiler FAQ2"Nvidia GPU (Pascal)(: B��No step marker observed and hence the step time is unknown. This may happen if (1) training steps are not instrumented (e.g., if you are not using Keras) or (2) the profiling duration is shorter than the step time. For (1), you need to add step instrumentation; for (2), you may try to profile longer.@dl019: Failed to load libcupti (is it installed and accessible?)