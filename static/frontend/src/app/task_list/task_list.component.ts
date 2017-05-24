import { Component } from '@angular/core';

@Component({
    selector: 'app-task_list',
    templateUrl: './task_list.component.html'
})
export class TaskListComponent {
    tasks = ['happy Task', 'sad Task'];
}
