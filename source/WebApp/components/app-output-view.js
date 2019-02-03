import Vue from 'vue';
import OutputViewSimple from './internal/app-output-view-simple.js';
import OutputViewMemory from './internal/app-output-view-memory.js';
import OutputViewGraph from './internal/app-output-view-graph.js';

Vue.component('app-output-view', {
    props: {
        output: Array
    },
    template: '#app-output-view',
    components: {
        'sub-output-view-simple': OutputViewSimple,
        'sub-output-view-memory': OutputViewMemory,
        'sub-output-view-graph': OutputViewGraph
    }
});